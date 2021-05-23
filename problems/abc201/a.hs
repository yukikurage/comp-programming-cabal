-----------------
-- GHC Options --
-----------------

{-# OPTIONS_GHC -O2                       #-}
{-# OPTIONS_GHC -Wno-unused-imports       #-}

-------------------------
-- Language Extensions --
-------------------------

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Main where

------------------
-- Import Lists --
------------------

import qualified Control.Monad                 as M
import qualified Control.Monad.Primitive       as Prim
import qualified Control.Monad.ST              as ST
import qualified Data.Bits                     as Bits
import qualified Data.ByteString.Char8         as BS
import qualified Data.Char                     as Char
import qualified Data.Complex                  as Comp
import qualified Data.Foldable                 as Foldable
import qualified Data.Function                 as Func
import qualified Data.Heap                     as Heap
import qualified Data.IORef                    as IORef
import qualified Data.IntPSQ                   as PSQ
import qualified Data.Ix                       as Ix
import qualified Data.List                     as L
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Primitive.MutVar         as MutVar
import qualified Data.Proxy                    as Proxy
import qualified Data.Ratio                    as Ratio
import qualified Data.STRef                    as STRef
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import qualified Data.Tree                     as Tree
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Merge  as VAM
import qualified Data.Vector.Algorithms.Radix  as VAR
import qualified Data.Vector.Algorithms.Search as VAS
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed.Base      as VUB
import qualified Data.Vector.Unboxing          as VU
import qualified Data.Vector.Unboxing.Mutable  as VUM
import qualified Debug.Trace                   as Trace
import qualified GHC.TypeNats                  as TypeNats
import           Prelude                       hiding (print)
import qualified Test.QuickCheck               as QC

----------
-- Main --
----------

main :: IO ()
main = do
  xs <- get @(VU.Vector Int)
  let
    ys = VU.modify VAM.sort xs
  print @BS.ByteString $ if ys ! 1 - ys ! 0 == ys ! 2 - ys ! 1
    then "Yes"
    else "No"

-------------
-- Library --
-------------

---------
-- I/O --
---------

-- | ex) getLn @Int @VU.Vector n, getLn @[Int] @V.Vector n
getLn :: (ReadBS a, VG.Vector v a) => Int -> IO (v a)
getLn n = VG.replicateM n get

-- | ex) get @Int, get @(VU.Vector) ..
get :: ReadBS a => IO a
get = readBS <$> BS.getLine

-- | 改行あり出力
print :: ShowBS a => a -> IO ()
print = BS.putStrLn . showBS

-- | 改行なし出力
printF :: ShowBS a => a -> IO ()
printF = BS.putStr . showBS

---------------
-- Read/Show --
---------------

class ReadBS a where
  readBS :: BS.ByteString -> a

  {-# MINIMAL readBS #-}
-- ^ ByteString版Read

class ShowBS a where
  showBS :: a -> BS.ByteString

  {-# MINIMAL showBS #-}

-- こうした方がStringを経由しないので高速？
instance ReadBS Int where
  readBS s = case BS.readInt s of
    Just (x, _) -> x
    Nothing     -> error "readBS :: ByteString -> Int"

instance ReadBS Integer where
  readBS = fromInteger . readBS

instance ReadBS Double where
  readBS = read . BS.unpack

instance ReadBS BS.ByteString where
  readBS = id

instance (ReadBS a, VU.Unboxable a) => ReadBS (VU.Vector a) where
  readBS s = VG.fromList . map readBS . BS.words $ s

instance (ReadBS a) => ReadBS (V.Vector a) where
  readBS s = VG.fromList . map readBS . BS.words $ s

instance ReadBS a => ReadBS [a] where
  readBS s = map readBS . BS.words $ s

instance (ReadBS a, ReadBS b) => ReadBS (a, b) where
  readBS (BS.words -> [a, b]) = (readBS a, readBS b)
  readBS _ = error "Invalid Format :: readBS :: ByteString -> (a, b)"

instance (ReadBS a, ReadBS b, ReadBS c) => ReadBS (a, b, c) where
  readBS (BS.words -> [a, b, c]) = (readBS a, readBS b, readBS c)
  readBS _ = error "Invalid Format :: readBS :: ByteString -> (a, b)"

instance ShowBS Int where
  showBS = BS.pack . show

instance ShowBS Integer where
  showBS = BS.pack . show

instance ShowBS Double where
  showBS = BS.pack . show

instance ShowBS BS.ByteString where
  showBS = id

instance (ShowBS a, VU.Unboxable a) => ShowBS (VU.Vector a) where
  showBS = showVec

instance (ShowBS a) => ShowBS (V.Vector a) where
  showBS = showVec

instance ShowBS a => ShowBS [a] where
  showBS = BS.pack . unwords . map (BS.unpack . showBS)

instance (ShowBS a, ShowBS b) => ShowBS (a, b) where
  showBS (a, b) = showBS a `BS.append` " " `BS.append` showBS b

instance (ShowBS a, ShowBS b, ShowBS c) => ShowBS (a, b, c) where
  showBS (a, b, c) = showBS a `BS.append` " " `BS.append` showBS b
    `BS.append` showBS c

showVec :: (VG.Vector v a, ShowBS a) => v a -> BS.ByteString
showVec xs
  | VG.null xs = ""
  | otherwise = BS.concat [f i | i <- [0 .. 2 * VG.length xs - 2]]
  where
  f i
    | even i = showBS $ xs ! (i `div` 2)
    | otherwise = " "

------------
-- ModInt --
------------

newtype Mod a (p :: TypeNats.Nat) = ModInt a deriving (Eq, Show)

instance VUB.Unbox a => VU.Unboxable (Mod a p) where
  type Rep (Mod a p) = a

instance ShowBS a => ShowBS (Mod a p) where
  showBS (ModInt x) = showBS x

instance (TypeNats.KnownNat p, Integral a) => Num (Mod a p) where
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  negate (ModInt x) = ModInt $ - x `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  abs = id
  signum _ = 1
  fromInteger n = ModInt $ fromInteger n `mod` p where
    p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)

instance (TypeNats.KnownNat p, Integral a) => Fractional (Mod a p) where
  recip (ModInt n)
    | gcd n p /= 1 = error "recip :: Mod a p -> Mod a p : The inverse element does not exist."
    | otherwise = ModInt . fst $ extendedEuc n (-p)
    where
    p = fromIntegral $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  fromRational r = ModInt n / ModInt d where
    n = fromInteger $ Ratio.numerator r
    d = fromInteger $ Ratio.denominator r

----------------------------
-- Coordinate Compression --
----------------------------

comp :: (VU.Unboxable  a, Ord a) => VU.Vector a -> (VU.Vector a, VU.Vector Int)
comp xs = (vals, comped) where
  vals = VU.uniq . VU.modify VAM.sort $ xs
  comped = ST.runST $ VU.thaw vals >>= \v -> VU.mapM (VAS.binarySearch v) xs

uncomp :: VU.Unboxable  a => VU.Vector a -> VU.Vector Int -> VU.Vector a
uncomp vals = VU.map (vals !)

------------------------
-- Laurent Polynomial --
------------------------

data LaurentPolynominal f = Laurent {lpLeft :: Int, lpCoefs :: V.Vector f} deriving Eq
-- ^ F[X, X^−1]を構成

instance ShowBS f => ShowBS (LaurentPolynominal f) where
  showBS l
    | V.null (lpCoefs l) = "0"
    | V.length (lpCoefs l) == 1 = showFirst
    | otherwise = showFirst `BS.append` " + "
      `BS.append` showBS l{lpLeft = lpLeft l + 1, lpCoefs = V.tail (lpCoefs l)}
    where
    showFirst
      | lpLeft l == 0 = showBS . V.head . lpCoefs $ l
      | otherwise = (showBS . V.head . lpCoefs $ l) `BS.append` " * X^" `BS.append` showBS (lpLeft l)

instance (Eq f, Num f) => Num (LaurentPolynominal f) where
  l1 + l2 = lpMold $ Laurent l xs where
    l = min (lpLeft l1) (lpLeft l2)
    r = max (lpRight l1) (lpRight l2)
    xs = V.map (\i -> l1 !^ i + l2 !^ i) [l .. r]
  l1 * l2 = lpMold $ Laurent l xs where
    l = lpLeft l1 + lpLeft l2
    r = lpRight l1 + lpRight l2
    xs = V.map f [l .. r]
    f i = V.foldr' (+) 0 . V.map (\j -> l1 !^ j * l2 !^ (i - j)) $ [lpLeft l1 .. lpRight l1]
  negate l = lpMold $ l{lpCoefs = V.map negate (lpCoefs l)}
  abs = id
  signum _ = 1
  fromInteger n = Laurent 0 [fromInteger n]

-- | (係数が0でない)最大の次数
lpRight :: LaurentPolynominal a -> Int
lpRight l = lpLeft l + V.length (lpCoefs l) - 1

(!^) :: Num p => LaurentPolynominal p -> Int -> p
(!^) (Laurent l xs) i
  | 0 <= j && j < V.length xs = xs ! j
  | otherwise = 0
  where
  j = i - l

-- | 左右の0成分を切ります
lpMold :: (Eq f, Num f) => LaurentPolynominal f -> LaurentPolynominal f
lpMold (Laurent l xs) = Laurent l' xs' where
  cutL = Maybe.fromMaybe (V.length xs + 1) $ V.findIndex (/= 0) xs
  xs' = V.takeWhile (/= 0) . V.drop cutL $ xs
  l' = l + cutL

lpX :: Num f => Int -> LaurentPolynominal f
lpX n = Laurent n [1]

lpF :: f -> LaurentPolynominal f
lpF n = Laurent 0 [n]

------------
-- InfInt --
------------

newtype Inf a = Inf (LaurentPolynominal a) deriving (Eq)

-- ↓バカ実装(?)
instance ReadBS a => ReadBS (Inf a) where
  readBS = Inf . lpF . readBS

instance ShowBS a => ShowBS (Inf a) where
  showBS (Inf a) = "Inf : " `BS.append` showBS a

instance (Eq a, Num a, Ord a) => Num (Inf a) where
  (Inf l1) + (Inf l2) = Inf $ l1 + l2
  (Inf l1) * (Inf l2) = Inf $ l1 * l2
  negate (Inf l1) = Inf $ negate l1
  abs n = if n >= 0 then n else - n
  signum n = if n >= 0 then 1 else - 1
  fromInteger = Inf . fromInteger

instance (Num a, Ord a) => Ord (Inf a) where
  (Inf l1) <= (Inf l2) = loop l
    where
    loop i
      | i > r = True
      | l1 !^ i == l2 !^ i = loop (i + 1)
      | otherwise = l1 !^ i <= l2 !^ i
    l = min (lpLeft l1) (lpLeft l2)
    r = max (lpRight l1) (lpRight l2)

_d :: Num a => Inf a
_d = Inf $ lpX 1

infinity :: Num a => Inf a
infinity = Inf $ lpX -1

fromInf :: (Eq a, Num a) => Inf a -> Maybe a
fromInf (Inf l)
  | V.length xs == 1 && left == 0 = Just $ xs ! 0
  | otherwise = Nothing
   where
  Laurent left xs = lpMold l

------------------
-- Disjoint Set --
------------------

type DisjointSet = VU.Vector Int
data DisjointSetM m = DSet
  {dsParents :: VUM.MVector m Int, dsDepths ::VUM.MVector m Int}

dsFromEdges :: Int -> VU.Vector (Int, Int) -> VU.Vector Int
dsFromEdges n edges = VU.create do
  ds <- newDSet n
  VU.forM_ edges $ uncurry (union ds)
  return $ dsParents ds

newDSet :: Prim.PrimMonad m => Int -> m (DisjointSetM (Prim.PrimState m))
newDSet n = DSet <$> VU.thaw (VU.generate n id) <*> VUM.replicate n 1

root :: DisjointSet -> Int -> Int
root xs i
  | xs ! i == i = i
  | otherwise = root xs $ xs ! i

find :: DisjointSet -> Int -> Int -> Bool
find xs i j = root xs i == root xs j

-- | ルートを調べる時につなぎ直す
rootM :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> m Int
rootM ds i = VUM.read (dsParents ds) i >>= \p -> if p == i
  then return i
  else rootM ds p >>= \r -> VUM.write (dsParents ds) i r >> return r

union :: Prim.PrimMonad m =>
  DisjointSetM (Prim.PrimState m) -> Int -> Int -> m ()
union ds i j = do
  rooti <- rootM ds i
  rootj <- rootM ds j
  depi <- VUM.read (dsDepths ds) rooti
  depj <- VUM.read (dsDepths ds) rootj
  if
    | depi == depj -> VUM.modify (dsDepths ds) (+ 1) rooti >>
    VUM.write (dsParents ds) rootj rooti
    | depi > depj -> VUM.write (dsParents ds) rootj rooti
    | otherwise  -> VUM.write (dsParents ds) rooti rootj

findM :: Prim.PrimMonad m =>
  DisjointSetM (Prim.PrimState m) -> Int -> Int -> m Bool
findM ds i j = (==) <$> rootM ds i <*> rootM ds j

----------------------------
-- Monadic Priority Queue --
----------------------------

-- 優先度付きキュー(IntPSQ)のモナディックな実装(いらないかも)
type PriorityQueue p = PSQ.IntPSQ p ()
type MPriorityQueue s p = MutVar.MutVar s (PriorityQueue p)

mpqNew :: (Prim.PrimMonad m, Ord p) => m (MPriorityQueue (Prim.PrimState m) p)
mpqNew = MutVar.newMutVar PSQ.empty

mpqMinView ::  (Prim.PrimMonad m, Ord p) => MPriorityQueue (Prim.PrimState m) p -> m (Maybe Int)
mpqMinView mpq = do
  pq <- MutVar.readMutVar mpq
  case PSQ.minView pq of
    Nothing             -> return Nothing
    Just (n, _, _, pq') -> MutVar.writeMutVar mpq pq' >> return (Just n)

mpqInsert :: (Prim.PrimMonad m, Ord p) => MPriorityQueue (Prim.PrimState m) p -> Int -> p -> m ()
mpqInsert mpq n p = MutVar.modifyMutVar' mpq (PSQ.insert n p ())

-----------
-- Graph --
-----------
type Graph a = V.Vector [(Int, a)]
type UGraph = Graph ()

gEmpty :: Graph a
gEmpty = V.singleton []

gFromEdges :: VU.Unboxable  a => Int -> VU.Vector (Int, Int, a) -> Graph a
gFromEdges n edges = ST.runST do
  v <- VM.replicate n []
  VU.forM_ edges \(i, j, a) -> VM.modify v ((j, a):) i
  V.freeze v

gReverse :: Graph a -> Graph a
gReverse g = ST.runST do
  let
    n = V.length g
  v <- VM.replicate n []
  V.forM_ [0 .. n - 1] \i -> M.forM_ (g ! i) \(j, a) -> VM.modify v ((i, a) :) j
  V.freeze v

dijkstra :: Graph Int -> Int -> V.Vector (Inf Int)
dijkstra g i = ST.runST do
  let
    n = V.length g
  d <- VM.new n
  q <- mpqNew
  V.forM_ [0 .. n - 1] \j -> let dj = if i == j then 0 else infinity in
    VM.write d j dj >> mpqInsert q j dj
  while $ mpqMinView q >>= \case
    Nothing -> return False
    Just u -> do
      dist_u <- VM.read d u
      M.forM_ (g ! u) \(v,len) -> do
        dist_v <- VM.read d v
        let
          alt = dist_u + fromIntegral len
        M.when (dist_v > alt) $ VM.write d v alt >> mpqInsert q v alt
      return True
  V.freeze d

dfs :: Graph a -> Int -> Tree.Tree Int
dfs g i = ST.runST do
  reached <- VUM.replicate (V.length g) False
  let
    loop now = do
      VUM.write reached now True
      nexts <- M.filterM (fmap not . VUM.read reached) . map fst $ g ! now
      childs <- M.mapM loop nexts
      return $ Tree.Node now childs
  loop i

------------
-- Others --
------------

while :: Monad m => m Bool -> m ()
while f = f >>= \frag -> M.when frag $ while f

divisor :: Integral a => a -> [a]
divisor n
  | n <= 0 = error
    "divisor : Definition range does not include negative numbers or zero"
  | otherwise = half ++ rev where
  mid = floor . sqrt @Double . fromIntegral $ n
  half = filter ((== 0) . mod n) [1 .. mid]
  rev = reverse . map (n `div`)
    . (if mid ^ (2 :: Int) == n then init else id) $ half

primeFact ::
  forall a b. (Integral a, Integral b) => a -> [(a, b)]
primeFact 1 = []
primeFact n
  | n == 1 = []
  | n <= 0 = error
    "primefact : Definition range does not include negative numbers or zero"
  | otherwise = case L.find ((== 0) . mod n) ([2 .. mid] :: [a]) of
  Nothing -> [(n, 1)]
  Just p  -> (p, m) : primeFact next where
    m = loop n p
    next = n `div` (p ^ m)
  where
  loop m q
    | m `mod` q == 0 = 1 + loop (m `div` q)  q
    | otherwise = 0
  mid = floor . sqrt @Double . fromIntegral $ n

primes :: forall a. Integral a => a -> [a]
primes n
  | n <= 1 = []
  | otherwise = filter ((frags !) . fromIntegral) [2 .. n] where
  frags = VU.create do
    v <- VUM.replicate (fromIntegral (n + 1)) True
    VUM.write v 0 False
    VUM.write v 1 False
    VU.forM_ [2 .. floor . sqrt @Double . fromIntegral $ n] \i -> do
      frag <- VUM.read v i
      M.when frag $ VU.forM_ [2 * i, 3 * i .. fromIntegral n] \j -> VUM.write v j False
    return v

-- | 拡張されたユークリッドの互除法
-- | ax + by = gcd a b を解く
extendedEuc :: (Integral b) => b -> b -> (b, b)
extendedEuc a b
  | a >= 0 && b == 0 = (1, 0)
  | a < 0 && b == 0 = (-1, 0)
  | otherwise = (t, s - q * t) where
  (q, r) = divMod a b
  (s, t) = extendedEuc b r

{-
prop_extendedEuc :: Integer -> Integer -> Bool
prop_extendedEuc a b = a * c + b * d == gcd a b where
  (c, d) = extendedEuc a b
-}
{-
generateArray :: (A.IArray a1 e, A.Ix a2) => (a2, a2) -> (a2 -> e) -> a1 a2 e
generateArray ind f = A.listArray ind . map f . A.range $ ind
-}
