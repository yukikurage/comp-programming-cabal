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
{-# LANGUAGE RankNTypes          #-}
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

import           Control.Arrow                 ((>>>))
import qualified Control.Monad                 as M
import qualified Control.Monad.Primitive       as Prim
import qualified Control.Monad.ST              as ST
import qualified Data.Array.IArray             as A
import qualified Data.Array.IO                 as AIO
import qualified Data.Array.ST                 as AST
import qualified Data.Array.Unboxed            as AU
import qualified Data.Bits                     as Bits
import qualified Data.ByteString.Char8         as BS
import qualified Data.Char                     as Char
import qualified Data.Complex                  as Comp
import qualified Data.Foldable                 as Foldable
import qualified Data.Function                 as Func
import qualified Data.Heap                     as Heap
import qualified Data.IORef                    as IORef
import qualified Data.IntPSQ                   as PSQueue
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
import           Prelude                       hiding (print, (!!))

----------
-- Main --
----------

main :: IO ()
main = do
  n :: Double <- fromIntegral <$> get @Int
  let
    leftMax :: Int = floor $ sqrt n
    left :: Int = sum $ flip map [1 .. leftMax] \x ->
      floor $ n / fromIntegral x

    rightMax = floor (n / fromIntegral leftMax) - 1
    right = sum $ flip map [1 .. rightMax] \x ->
      x * (floor (n / fromIntegral x) - floor (n / (fromIntegral x + 1)))

  print $ right + left

-- test :: Int -> Int
-- test n = VU.sum $ VU.generate n \x -> floor $ fromIntegral n / fromIntegral (x + 1)

-------------
-- Library --
-------------

---------
-- I/O --
---------

-- | ex) get @Int, get @(VU.Vector) ..
get :: ReadBS a => IO a
get = readBS <$> BS.getLine

-- | ex) getLn @Int @VU.Vector n, getLn @[Int] @V.Vector n
getLines :: ReadBSLines a => Int -> IO a
getLines n = readBSLines . BS.unlines <$> M.replicateM n BS.getLine

-- | 改行なし出力
output :: ShowBS a => a -> IO ()
output = BS.putStr . showBS

-- | 改行なし出力
outputLines :: ShowBSLines a => a -> IO ()
outputLines = BS.putStr . showBSLines

-- | 改行あり出力
print :: ShowBS a => a -> IO ()
print = BS.putStrLn . showBS

-- | 改行あり出力
printLines :: ShowBSLines a => a -> IO ()
printLines = BS.putStrLn . showBSLines

---------------
-- Read/Show --
---------------

-- | BS版Read
class ReadBS a where
  readBS :: BS.ByteString -> a

class ShowBS a where
  showBS :: a -> BS.ByteString

instance ReadBS Int where
  readBS s = case BS.readInt s of
    Just (x, _) -> x
    Nothing     -> error "readBS :: BS -> Int"

instance ReadBS Integer where
  readBS = fromIntegral . (readBS @Int)

instance ReadBS Double where
  readBS = read . BS.unpack

instance ReadBS BS.ByteString where
  readBS = id

instance (ReadBS a, VU.Unboxable a) => ReadBS (VU.Vector a) where
  readBS = readVec

instance (ReadBS a) => ReadBS (V.Vector a) where
  readBS = readVec

instance ReadBS a => ReadBS [a] where
  readBS = map readBS . BS.words

instance (ReadBS a, ReadBS b) => ReadBS (a, b) where
  readBS (BS.words -> [a, b]) = (readBS a, readBS b)
  readBS _
    = error "Invalid Format :: readBS :: BS -> (a, b)"

instance (ReadBS a, ReadBS b, ReadBS c) => ReadBS (a, b, c) where
  readBS (BS.words -> [a, b, c]) = (readBS a, readBS b, readBS c)
  readBS _ = error "Invalid Format :: readBS :: BS -> (a, b, c)"

instance (ReadBS a, ReadBS b, ReadBS c, ReadBS d) => ReadBS (a, b, c, d) where
  readBS (BS.words -> [a, b, c, d])
    = (readBS a, readBS b, readBS c, readBS d)
  readBS _
    = error "Invalid Format :: readBS :: BS -> (a, b, c, d)"

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
  showBS = BS.unwords . map showBS

instance (ShowBS a, ShowBS b) => ShowBS (a, b) where
  showBS (a, b) =
    showBS a
    `BS.append`
    " "
    `BS.append`
    showBS b

instance (ShowBS a, ShowBS b, ShowBS c) => ShowBS (a, b, c) where
  showBS (a, b, c) =
    showBS a
    `BS.append`
    " "
    `BS.append`
    showBS b
    `BS.append`
    " "
    `BS.append`
    showBS c

instance (ShowBS a, ShowBS b, ShowBS c, ShowBS d) => ShowBS (a, b, c, d) where
  showBS (a, b, c, d) =
    showBS a
    `BS.append`
    " "
    `BS.append`
    showBS b
    `BS.append`
    " "
    `BS.append`
    showBS c
    `BS.append`
    " "
    `BS.append`
    showBS d

readVec :: (VG.Vector v a, ReadBS a) => BS.ByteString -> v a
readVec = VG.fromList . readBS

showVec :: (VG.Vector v a, ShowBS a) => v a -> BS.ByteString
showVec = showBS . VG.toList

class ReadBSLines a where
  readBSLines :: BS.ByteString -> a

class ShowBSLines a where
  showBSLines :: a -> BS.ByteString

instance ReadBS a => ReadBSLines [a] where
  readBSLines = map readBS . BS.lines

instance (ReadBS a, VU.Unboxable a) => ReadBSLines (VU.Vector a) where
  readBSLines = readVecLines

instance ReadBS a => ReadBSLines (V.Vector a) where
  readBSLines = readVecLines

instance ReadBSLines BS.ByteString where
  readBSLines = id

instance ShowBS a => ShowBSLines [a] where
  showBSLines = BS.unwords . map showBS

instance (ShowBS a, VU.Unboxable a) => ShowBSLines (VU.Vector a) where
  showBSLines = showVecLines

instance ShowBS a => ShowBSLines (V.Vector a) where
  showBSLines = showVecLines

instance ShowBSLines BS.ByteString where
  showBSLines = id

readVecLines :: (VG.Vector v a, ReadBS a) => BS.ByteString -> v a
readVecLines = VG.fromList . readBSLines

showVecLines :: (VG.Vector v a, ShowBS a) => v a -> BS.ByteString
showVecLines = showBSLines . VG.toList

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
    | gcd n p /= 1 =
      error "recip :: Mod a p -> Mod a p : The inverse element does not exist."
    | otherwise = ModInt . fst $ extendedEuc n (-p)
    where
    p = fromIntegral $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  fromRational r = ModInt n / ModInt d where
    n = fromInteger $ Ratio.numerator r
    d = fromInteger $ Ratio.denominator r

------------
-- InfInt --
------------

{-
例えば
[1, 3, 2, 5] なら 1 + 3ω + 2ω^2 + 5ω^3
-}

data ViewInf a = Infinity | Finity a

newtype Inf a = Inf (V.Vector a) deriving Eq

instance ReadBS a => ReadBS (Inf a) where
  readBS = Inf . readBS

instance ShowBS a => ShowBS (Inf a) where
  showBS (Inf xs) = showBS xs

instance (Num a, Ord a) => Num (Inf a) where
  (Inf xs) + (Inf ys) = infMold . Inf $ zs
    where
    lx = V.length xs - 1
    ly = V.length ys - 1
    zs = V.map f [0 .. max lx ly]
    f i = xs .! i + ys .! i
  (Inf xs) * (Inf ys) = infMold . Inf $ zs
    where
    lx = V.length xs - 1
    ly = V.length ys - 1
    zs = V.map f [0 .. lx + ly]
    f i = V.sum . V.map g $ [0 .. i]
      where
      g j = xs .! j * ys .! (i - j)
  negate (Inf xs) = Inf . V.map negate $ xs
  abs n = if n >= 0 then n else - n
  signum n = if n >= 0 then 1 else - 1
  fromInteger n = Inf [fromInteger n]

instance (Num a, Ord a) => Ord (Inf a) where
  compare (Inf xs) (Inf ys)
    | l == -1 = EQ
    | xs .! l == ys .! l = compare (Inf (V.init xs)) (Inf (V.init ys))
    | otherwise = compare (xs .! l) (ys .! l)
    where
    l = max (V.length xs) (V.length ys) - 1

(.!) :: (VG.Vector v p, Num p) => v p -> Int -> p
xs .! i
  | i >= 0 && i < VG.length xs = xs ! i
  | otherwise = 0

infinity :: Num a => Inf a
infinity = Inf [0, 1]

fromInf :: (Eq a, Num a) => Inf a -> ViewInf a
fromInf (Inf xs)
  | V.length xs == 1 = Finity $ xs ! 0
  | otherwise = Infinity

infMold :: (Num a, Eq a) => Inf a -> Inf a
infMold (Inf xs) = Inf . dropWhileRev (== 0) $ xs

dropWhileRev :: VG.Vector v a => (a -> Bool) -> v a -> v a
dropWhileRev f xs
  | VG.null xs = VG.empty
  | f x = dropWhileRev f xs'
  | otherwise = xs
  where
  x = VG.last xs
  xs' = VG.init xs

------------------
-- Disjoint Set --
------------------

type DisjointSet = VU.Vector Int
data DisjointSetM m = DSet
  {dsParents :: VUM.MVector m Int, dsDepths :: VUM.MVector m Int}

dsFromEdges :: Int -> VU.Vector (Int, Int) -> DisjointSet
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

findM :: Prim.PrimMonad m =>
  DisjointSetM (Prim.PrimState m) -> Int -> Int -> m Bool
findM ds i j = (==) <$> rootM ds i <*> rootM ds j

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

----------------------------
-- Monadic Priority Queue --
----------------------------

type MPSQueue m p v = MutVar.MutVar m (PSQueue.IntPSQ p v)

mpsqNull :: Prim.PrimMonad m => MPSQueue (Prim.PrimState m) p v -> m Bool
mpsqNull q = PSQueue.null <$>  MutVar.readMutVar q

mpsqEmpty :: Prim.PrimMonad m => m (MPSQueue (Prim.PrimState m) p v)
mpsqEmpty = MutVar.newMutVar PSQueue.empty

mpsqSingleton ::
  Prim.PrimMonad m
  => Ord p
  => Int -> p -> v -> m (MPSQueue (Prim.PrimState m) p v)
mpsqSingleton k p v = MutVar.newMutVar $ PSQueue.singleton k p v

mpsqInsert ::
  Prim.PrimMonad m
  => Ord p
  => Int -> p -> v -> MPSQueue (Prim.PrimState m) p v -> m ()
mpsqInsert k p v = flip MutVar.modifyMutVar' (PSQueue.insert k p v)

-- | 要素は削除せず，優先度が一番小さいものを取り出す
mpsqFindMin ::
  Prim.PrimMonad m
  => Ord p
  => MPSQueue (Prim.PrimState m) p v -> m (Maybe (Int, p, v))
mpsqFindMin q = PSQueue.findMin <$> MutVar.readMutVar q

-- | 要素を削除して，優先度が一番小さいものを取り出す
mpsqMinView ::
  Prim.PrimMonad m
  => Ord p
  => MPSQueue (Prim.PrimState m) p v -> m (Maybe (Int, p, v))
mpsqMinView q = do
  res <- PSQueue.minView <$> MutVar.readMutVar q
  case res of
    Nothing -> return Nothing
    Just (k, p, v, q') -> do
      MutVar.writeMutVar q q'
      return $ Just (k, p, v)

-----------
-- Graph --
-----------

type Graph a = V.Vector [(Int, a)] --aは辺の情報
type UGraph = Graph ()

gEmpty :: Graph a
gEmpty = V.singleton []

gFromEdges :: VU.Unboxable  a => Int -> VU.Vector (Int, Int, a) -> Graph a
gFromEdges n edges = ST.runST do
  v <- VM.replicate n []
  VU.forM_ edges \(i, j, a) -> VM.modify v ((j, a):) i
  V.freeze v

-- | 辺をすべて反転させる
gReverse :: Graph a -> Graph a
gReverse g = ST.runST do
  let
    n = V.length g
  v <- VM.replicate n []
  V.forM_ [0 .. n - 1] \i -> M.forM_ (g ! i) \(j, a)
    -> VM.modify v ((i, a) :) j
  V.freeze v

dijkstra :: Graph Int -> Int -> V.Vector (Inf Int)
dijkstra g i = V.create do
  let
    n = V.length g

  -- 辺の距離の初期化
  dists <- VM.replicate n infinity
  VM.write dists i 0

  -- キューの初期化
  queue <- mpsqSingleton i 0 ()

  let
    -- 頂点情報のアップデート処理
    update v alt = do
      VM.write dists v alt
      mpsqInsert v alt () queue

    -- 確定した頂点を取り出したときの処理
    processing u = do
      dist_u <- VM.read dists u
      M.forM_ (g ! u) (\(v, cost) -> do
        dist_v <- VM.read dists v
        let
          alt = dist_u + fromIntegral cost
        M.when (dist_v > alt) $ update v alt
        )

  while do
    res <- mpsqMinView queue
    case res of
      Nothing             -> return False
      Just (u, _, _) -> do
        processing u
        return True
  return dists

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

----------
-- Maze --
----------

type Maze = AU.Array (Int, Int) Char
-- ^ 競プロでよく出るCharの迷路

mzHeight :: Maze -> Int
mzHeight = (+1) . fst . snd . AU.bounds

mzWidth :: Maze -> Int
mzWidth = (+1) . snd . snd . AU.bounds

type Rules a = Char -> Char -> Maybe a

readMaze :: BS.ByteString  -> Maze
readMaze str = AU.listArray ((0, 0), (h - 1, w - 1))
  $ concatMap (BS.unpack . BS.take w) strs
  where
  strs = BS.lines str
  h = length strs
  w = minimum $ map BS.length strs

getMaze :: Int -> IO Maze
getMaze h = readMaze <$> getLines h

mazeToGraph :: VUM.Unboxable a => Rules a -> Maze -> Graph a
mazeToGraph rules maze = gFromEdges (h * w) $ VU.fromList $
  Maybe.catMaybes $
  [edge |
  i <- [0 .. h - 1],
  j <- [0 .. w - 1],
  (i', j') <- [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)],
  i' >= 0,
  i' < h,
  j' >= 0,
  j' < w,
  let c = maze AU.! (i, j),
  let c' = maze AU.! (i', j'),
  let edge = (h * i + j, h * i' + j',) <$> rules c c']
  where
  h = mzHeight maze
  w = mzWidth maze

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

-- | 素数を取得
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
      M.when frag $ VU.forM_ [2 * i, 3 * i .. fromIntegral n] \j
        -> VUM.write v j False
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
