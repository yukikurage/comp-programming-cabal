{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Array.IArray             as A
import qualified Data.Array.IO                 as AIO
import qualified Data.Array.MArray             as AM
import qualified Data.Array.ST                 as AST
import qualified Data.Array.Unboxed            as AU
import           Data.Bits
import qualified Data.ByteString.Char8         as BS
import           Data.Char
import           Data.Complex
import qualified Data.Graph                    as Graph
import qualified Data.Heap                     as Heap
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Primitive.MutVar
import           Data.Proxy
import           Data.STRef
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import qualified Data.Tree                     as Tree
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Merge  as VAM
import           Data.Vector.Algorithms.Search
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Debug.Trace
import           GHC.TypeNats
import           Prelude                       hiding (negate, recip, (*), (+),
                                                (-), (/), (^), (^^))
import qualified Prelude

----------
-- Main --
----------

main :: IO ()
main = do
  [n, q] <- get @[Int]
  edges' <- map (\(i, j) -> (i - 1, j - 1)) <$> replicateM (n - 1) (get @(Int, Int))
  qs <- getLn @(Int, Int) @VU.Vector $ q
  let
    edges = edges' ++ map (\(i, j) -> (j, i)) edges'
    tr = head . flip Graph.dfs [0] . Graph.buildG (0, n - 1) $ edges
    counts = VU.create do
      v <- VUM.replicate n 0
      VU.forM_ qs \(i, m) -> VUM.modify v (+ m) (i - 1)
      return v
  v <- VUM.replicate n 0
  let
    solve parent (Tree.Node i next) = do
      parentVal <- VUM.read v parent
      VUM.write v i $ parentVal + counts ! i
      forM_ next $ solve i
  solve 0 tr
  BS.putStrLn . showVec =<< VU.freeze v
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

-- | 改行なし出力
printF :: Show a => a -> IO ()
printF = putStr . show

showVec :: (VG.Vector v a, Show a) => v a -> BS.ByteString
showVec xs
  | VG.null xs = ""
  | otherwise = BS.concat [f i | i <- [0 .. 2 * VG.length xs - 2]] where
    f i
      | even i = BS.pack . show $ xs ! (i `div` 2)
      | otherwise = " "

---------------
-- Read/Show --
---------------

class ReadBS a where
  readBS :: BS.ByteString -> a
-- ^ ByteString版Read

instance (ReadBS a, ReadBS b) => ReadBS (a, b) where
  readBS s = case BS.words s of
    [a, b] -> (readBS a, readBS b)
    _      -> error "Invalid Format :: readBS :: ByteString -> (a, b)"

instance (ReadBS a, ReadBS b, ReadBS c) => ReadBS (a, b, c) where
  readBS s = case BS.words s of
    [a, b, c] -> (readBS a, readBS b, readBS c)
    _         -> error "Invalid Format :: readBS :: ByteString -> (a, b, c)"

-- こうした方がStringを経由しないので高速？
instance ReadBS Int where
  readBS s = case BS.readInt s of
    Just (x, _) -> x
    Nothing     -> error "readBS :: ByteString -> Int"

instance ReadBS Double where
  readBS = read . BS.unpack

instance (ReadBS a, VU.Unbox a) => ReadBS (VU.Vector a) where
  readBS s = VG.fromList . map readBS . BS.words $ s

instance (ReadBS a) => ReadBS (V.Vector a) where
  readBS s = VG.fromList . map readBS . BS.words $ s

instance ReadBS a => ReadBS [a] where
  readBS s = map readBS . BS.words $ s

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

class (Ring a) => Field a where
  one :: a
  recip :: a -> a
  (/) :: a -> a -> a
  x / y = x * recip y
  recip x = one / x

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

instance Field Float where
  recip = Prelude.recip
  one = 1

instance Field Double where
  recip = Prelude.recip
  one = 1

repsquares :: Integral n => (a -> a -> a) -> a -> n -> a
repsquares f = loop where
  loop a n
    | n == 1 = a
    | n == 2 = f a a
    | n >= 1 && even n = loop (loop a (n `div` 2)) 2
    | n >= 1 && odd n = f a (loop a (n Prelude.- 1))
    | otherwise = error "repsquares : Invalid Integer"

(^) :: (Integral n, Ring a) => a -> n -> a
a ^ n = repsquares (*) a n

(^^) :: (Integral n, Field a) => a -> n -> a
a ^^ n
  | n < 0 = recip $ a ^ (-n)
  | n == 0 = one
  | otherwise = a ^ n

------------
-- ModInt --
------------

newtype ModInt (p :: Nat) = ModInt Int deriving Eq

instance Show (ModInt p) where
  show (ModInt x) = show x

instance KnownNat p => Ring (ModInt p) where
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  zero = ModInt 0
  negate (ModInt x) = ModInt $ - x `mod` p where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)

instance KnownNat p => Field (ModInt p) where
  one = ModInt 1
  recip n = n ^ (p - 2) where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p) :: Int

modint :: forall p. KnownNat p => Int -> ModInt p
modint n = ModInt $ n `mod` p
  where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)

frommodint :: ModInt p -> Integer
frommodint (ModInt n) = toInteger n

-------------
-- Counter --
-------------

newtype Counter a = Counter (Map.Map a Int)

-- | O(log(n))
(<<+) :: Ord a => Counter a -> a -> Counter a
(<<+) (Counter cntr) a = Counter next where
  next = if Map.member a cntr then Map.adjust (+1) a cntr else Map.insert a 1 cntr

-- | O(log(n))
(<<-) :: Ord a => Counter a -> a -> Counter a
(<<-) (Counter cntr) a = Counter next where
  next = Map.update (\i -> if i - 1 == 0 then Nothing else Just (i - 1)) a cntr

-- | O(n)
cntrToList :: Counter a -> [(a, Int)]
cntrToList (Counter cntr) = Map.toList cntr

cntrEmpty :: Counter a
cntrEmpty = Counter Map.empty
