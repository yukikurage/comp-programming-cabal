-----------------
-- GHC Options --
-----------------

{-# OPTIONS_GHC -O2                       #-}
{-# OPTIONS_GHC -Wno-unused-imports       #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

------------------
-- Import Lists --
------------------

import qualified Control.Monad                 as M
import qualified Control.Monad.Primitive       as Prim
import qualified Control.Monad.ST              as ST
import qualified Data.Array.IArray             as A
import qualified Data.Array.IO                 as AIO
import qualified Data.Array.MArray             as AM
import qualified Data.Array.ST                 as AST
import qualified Data.Array.Unboxed            as AU
import qualified Data.Bits                     as Bits
import qualified Data.ByteString.Char8         as BS
import qualified Data.Char                     as Char
import qualified Data.Complex                  as Comp
import qualified Data.Foldable                 as Foldable
import qualified Data.Heap                     as Heap
import qualified Data.IORef                    as IORef
import qualified Data.IntPSQ                   as PSQ
import qualified Data.List                     as L
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Primitive.MutVar         as MutVar
import qualified Data.Proxy                    as Proxy
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
import qualified Data.Vector.Unboxing          as VU
import qualified Data.Vector.Unboxing.Mutable  as VUM
import qualified Debug.Trace                   as Trace
import qualified GHC.TypeNats                  as TypeNats
import           Prelude                       hiding (negate, print, recip,
                                                (*), (+), (-), (/), (^), (^^))
import qualified Prelude
import qualified Test.QuickCheck               as QC

----------
-- Main --
----------

main :: IO ()
main = do
  (n, m, t)<- get @(Int, Int, InfInt)
  xs <- get @(V.Vector InfInt)
  edges <- VU.map (\(i, j, c) -> (i - 1, j - 1, c)) <$> VU.replicateM m (get @(Int, Int, Int))
  let
    g = gFromEdges n edges
    sp = dijkstra g 0
    spRev = dijkstra (gReverse g) 0
    times = V.zipWith (+) sp spRev
  print . VU.maximum . VU.map (\i -> fromInfInt . max 0 $ (xs ! i) * (t - times ! i)) $ [0 .. n - 1]
  return ()

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

----------------
-- Ring/Field --
----------------

class Eq a => Ring a where
  (+), (-) :: a -> a -> a
  (*) :: a -> a -> a
  zero :: a
  negate :: a -> a
  x - y = x + negate y
  negate x = zero - x

  {-# MINIMAL ((+), (-), (*), zero) | ((+), negate, (*), zero) #-}

class Ring a => UnitaryRing a where
  one :: a
  {-# MINIMAL one #-}

class UnitaryRing a => Field a where
  recip :: a -> a
  (/) :: a -> a -> a
  x / y = x * recip y
  recip x = one / x

  {-# MINIMAL recip | (/) #-}

infixl 8 ^
infixl 8 ^^
infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /

instance Ring Int where
  (+) = (Prelude.+)
  (*) = (Prelude.*)
  zero = 0
  negate = Prelude.negate

instance Ring Integer where
  (+) = (Prelude.+)
  (*) = (Prelude.*)
  zero = 0
  negate = Prelude.negate

instance Ring Double where
  (+) = (Prelude.+)
  (*) = (Prelude.*)
  zero = 0
  negate = Prelude.negate

instance Ring Float where
  (+) = (Prelude.+)
  (*) = (Prelude.*)
  zero = 0
  negate = Prelude.negate

instance UnitaryRing Double where
  one = 1

instance UnitaryRing Float where
  one = 1

instance UnitaryRing Int where
  one = 1

instance UnitaryRing Integer where
  one = 1

instance Field Float where
  recip = Prelude.recip

instance Field Double where
  recip = Prelude.recip

repsquares ::(Integral n, Ring n, Ring a) => (a -> a -> a) -> a -> n -> a
repsquares f = loop where
  loop a n
    | n == 1 = a
    | n == 2 = f a a
    | n >= 1 && even n = loop (loop a (n `div` 2)) 2
    | n >= 1 && odd n = f a (loop a (n - 1))
    | otherwise = error "repsquares : Invalid Integer"

(^) :: (Integral n, Ring n, Ring a) => a -> n -> a
a ^ n = repsquares (*) a n

(^^) :: (Integral n, Ring n, Field a) => a -> n -> a
a ^^ n
  | n < 0 = recip $ a ^ (-n)
  | n == 0 = one
  | otherwise = a ^ n

------------
-- ModInt --
------------

newtype ModInt (p :: TypeNats.Nat) = ModInt Int deriving (Eq, Show)

instance VU.Unboxable (ModInt p) where
  type Rep (ModInt p) = Int

instance ShowBS (ModInt p) where
  showBS (ModInt x) = showBS x

instance TypeNats.KnownNat p => Ring (ModInt p) where
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p where
    p = fromInteger . toInteger $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p where
    p = fromInteger . toInteger $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  zero = ModInt 0
  negate (ModInt x) = ModInt $ - x `mod` p where
    p = fromInteger . toInteger $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)

instance TypeNats.KnownNat p => UnitaryRing (ModInt p) where
  one = ModInt 1

instance TypeNats.KnownNat p => Field (ModInt p) where
  recip n
    | gcd (fromIntegral . frommodint $ n) p /= 1 = error "recip :: ModInt p -> ModInt p : The inverse element does not exist."
    | otherwise = modint . fst $ extendedEuc (fromIntegral . frommodint $ n) (- p)  where
    p = fromInteger . toInteger $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p) :: Int
{-
prop_modrecip :: Int -> Bool
prop_modrecip n' = n' == 0 || n ^^ (-1 :: Int) * n == one where
  n = modint n' :: ModInt 1000000007
-}

modint :: forall p. TypeNats.KnownNat p => Int -> ModInt p
modint n = ModInt $ n `mod` p
  where
    p = fromInteger . toInteger $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)

frommodint :: ModInt p -> Integer
frommodint (ModInt n) = toInteger n

-------------
-- Counter --
-------------

newtype Counter a = Counter (Map.Map a Int) deriving (Eq, Show)

-- | O(log(n))
(<*) :: Ord a => Counter a -> a -> Counter a
(<*) (Counter cntr) a = Counter next where
  next = if Map.member a cntr
    then Map.adjust (+1) a cntr
    else Map.insert a 1 cntr

-- | O(log(n))
(*>) :: Ord a => Counter a -> a -> Counter a
(*>) (Counter cntr) a = Counter next where
  next = Map.update (\i -> if i - 1 == 0 then Nothing else Just (i - 1)) a cntr

-- | O(n)
cntrToList :: Counter a -> [(a, Int)]
cntrToList (Counter cntr) = Map.toList cntr

cntrEmpty :: Counter a
cntrEmpty = Counter Map.empty

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

instance Show f => Show (LaurentPolynominal f) where
  show l
    | V.null (lpCoefs l) = "zero"
    | V.length (lpCoefs l) == 1 = showFirst
    | otherwise = showFirst ++ " + "
      ++ show l{lpLeft = lpLeft l + 1, lpCoefs = V.tail (lpCoefs l)}
    where
    showFirst
      | lpLeft l == 0 = show . V.head . lpCoefs $ l
      | otherwise = (show . V.head . lpCoefs $ l) ++ " * X^" ++ show (lpLeft l)

instance Show f => ShowBS (LaurentPolynominal f) where
  showBS = BS.pack . show

instance Ring f => Ring (LaurentPolynominal f) where
  l1 + l2 = lpMold $ Laurent l xs where
    l = min (lpLeft l1) (lpLeft l2)
    r = max (lpRight l1) (lpRight l2)
    xs = V.map (\i -> lpCoefficient l1 i + lpCoefficient l2 i) [l .. r]
  l1 * l2 = lpMold $ Laurent l xs where
    l = lpLeft l1 + lpLeft l2
    r = lpRight l1 + lpRight l2
    xs = V.map f [l .. r]
    f i = V.foldr' (+) zero . V.map (\j -> lpCoefficient l1 j * lpCoefficient l2 (i - j)) $ [lpLeft l1 .. lpRight l1]
  negate l = lpMold $ l{lpCoefs = V.map negate (lpCoefs l)}
  zero = Laurent 0 []

instance UnitaryRing f => UnitaryRing (LaurentPolynominal f) where
  one = Laurent 0 [one]

-- | (係数が0でない)最大の次数
lpRight :: LaurentPolynominal a -> Int
lpRight l = lpLeft l + V.length (lpCoefs l) - 1

lpCoefficient :: Ring p => LaurentPolynominal p -> Int -> p
lpCoefficient (Laurent l xs) i
  | 0 <= j && j < V.length xs = xs ! j
  | otherwise = zero
  where
  j = i - l

-- | 左右の0成分を切ります
lpMold :: Ring f => LaurentPolynominal f -> LaurentPolynominal f
lpMold (Laurent l xs) = Laurent l' xs' where
  cutL = Maybe.fromMaybe (V.length xs + 1) $ V.findIndex (/= zero) xs
  xs' = V.takeWhile (/= zero) . V.drop cutL $ xs
  l' = l + cutL

lpX :: UnitaryRing f =>Int -> LaurentPolynominal f
lpX n = Laurent n [one]

lpF :: f -> LaurentPolynominal f
lpF n = Laurent 0 [n]

------------
-- InfInt --
------------

newtype InfInt = InfInt (LaurentPolynominal Int) deriving (Eq)

-- ↓バカ実装
instance ReadBS InfInt where
  readBS = InfInt . lpF . readBS

instance Show InfInt where
  show (InfInt l) = show l

instance ShowBS InfInt where
  showBS = BS.pack . show

instance Ring InfInt where
  (InfInt l1) + (InfInt l2) = InfInt $ l1 + l2
  (InfInt l1) * (InfInt l2) = InfInt $ l1 * l2
  zero = InfInt zero
  negate (InfInt l1) = InfInt $ negate l1

instance UnitaryRing InfInt where
  one = InfInt one

instance Ord InfInt where
  (InfInt l1) <= (InfInt l2) = loop l
    where
    loop i
      | i > r = True
      | lpCoefficient l1 i == lpCoefficient l2 i = loop (i + 1)
      | otherwise = lpCoefficient l1 i <= lpCoefficient l2 i
    l = min (lpLeft l1) (lpLeft l2)
    r = max (lpRight l1) (lpRight l2)

_d :: InfInt
_d = InfInt $ lpX 1

instance Num InfInt where
  (+) = (+)
  (*) = (*)
  (-) = (-)
  abs n = if n >= 0 then n else - n
  signum n = if n >= 0 then 1 else -1
  fromInteger =  InfInt . lpF . fromInteger

infinity :: InfInt
infinity = InfInt $ lpX -1

fromInfInt :: InfInt -> Int
fromInfInt (InfInt l) = lpCoefficient l 0

---------------------------------
-- Disjoint Set Data Structure --
---------------------------------

data DisjointSet m = DSet
  {dsParents :: VUM.MVector m Int, dsDepths ::VUM.MVector m Int}

newDSet :: Prim.PrimMonad m => Int -> m (DisjointSet (Prim.PrimState m))
newDSet n = DSet <$> VU.thaw (VU.generate n id) <*> VUM.replicate n 1

-- | ルートを調べる時につなぎ直す
root :: Prim.PrimMonad m => DisjointSet (Prim.PrimState m) -> Int -> m Int
root ds i = VUM.read (dsParents ds) i >>= \p -> if p == i
  then return i
  else root ds p >>= \r -> VUM.write (dsParents ds) i r >> return r

union :: Prim.PrimMonad m =>
  DisjointSet (Prim.PrimState m) -> Int -> Int -> m ()
union ds i j = do
  rooti <- root ds i
  rootj <- root ds j
  depi <- VUM.read (dsDepths ds) rooti
  depj <- VUM.read (dsDepths ds) rootj
  if
    | depi == depj -> VUM.modify (dsDepths ds) (+ 1) rooti >>
    VUM.write (dsParents ds) rootj rooti
    | depi > depj -> VUM.write (dsParents ds) rootj rooti
    | otherwise  -> VUM.write (dsParents ds) rooti rootj

find :: Prim.PrimMonad m =>
  DisjointSet (Prim.PrimState m) -> Int -> Int -> m Bool
find ds i j = (==) <$> root ds i <*> root ds j

----------------------------
-- Monadic Priority Queue --
----------------------------

-- 優先度付きキュー(IntPSQ)のモナディックな実装(いる？)
type PriorityQueue p = PSQ.IntPSQ p ()
type MPriorityQueue s p = MutVar.MutVar s (PriorityQueue p)
--関数使いまわし可能

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

dijkstra :: Graph Int -> Int -> V.Vector InfInt
dijkstra g i = ST.runST do
  let
    n = V.length g
  d <- VM.new n
  q <- mpqNew
  V.forM_ [0 .. n - 1] \j -> let dj = if i == j then zero else infinity in
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

------------
-- Others --
------------

while :: Monad m => m Bool -> m ()
while f = f >>= \frag -> M.when frag $ while f

divisor :: (Integral a, Ring a) => a -> [a]
divisor n
  | n <= 0 = error
    "divisor : Definition range does not include negative numbers or zero"
  | otherwise = half ++ rev where
  mid = floor . sqrt @Double . fromIntegral $ n
  half = filter ((== 0) . mod n) [1 .. mid]
  rev = reverse . map (n `div`)
    . (if mid ^ (2 :: Int) == n then init else id) $ half

primefact ::
  forall a b. (Integral a, Ring a, Integral b, Ring b) => a -> [(a, b)]
primefact 1 = []
primefact n
  | n == 1 = []
  | n <= 0 = error
    "primefact : Definition range does not include negative numbers or zero"
  | otherwise = case L.find ((== 0) . mod n) ([2 .. mid] :: [a]) of
  Nothing -> [(n, 1)]
  Just p  -> (p, m) : primefact next where
    m = loop n p
    next = n `div` (p ^ m)
  where
  loop m q
    | m `mod` q == 0 = 1 + loop (m `div` q)  q
    | otherwise = 0
  mid = floor . sqrt @Double . fromIntegral $ n

-- うまく多相に出来ない
primes :: Int -> [Int]
primes n
  | n <= 1 = []
  | otherwise = filter ((frags !) . fromIntegral) [2 .. n] where
  frags = VU.create do
    v <- VUM.replicate (fromIntegral (n + 1)) True
    VUM.write v 0 False
    VUM.write v 1 False
    VU.forM_ [2 .. floor . sqrt @Double . fromIntegral $ n] \i -> do
      frag <- VUM.read v i
      M.when frag $ VU.forM_ [2 * i, 3 * i .. n] \j -> VUM.write v j False
    return v

-- | 拡張されたユークリッドの互除法
-- | ax + by = gcd a b を解きます
extendedEuc :: (Integral b, Ring b) => b -> b -> (b, b)
extendedEuc a b
  | a >= 0 && b ==0 = (1, 0)
  | a < 0 && b == 0 = (-1, 0)
  | otherwise = (t, s - q * t) where
  (q, r) = divMod a b
  (s, t) = extendedEuc b r

{-
prop_extendedEuc :: Integer -> Integer -> Bool
prop_extendedEuc a b = a * c + b * d == gcd a b where
  (c, d) = extendedEuc a b
-}
