{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NegativeLiterals     #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
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
  [h, w] <- get @[Int]
  xs <- A.listArray ((0, 0), (h - 1, w - 1)) . concat <$>
    replicateM h (BS.unpack <$> BS.getLine)
    :: IO (AU.UArray (Int, Int) Char)
  yoko <- AIO.newArray ((0, 0), (h - 1, w - 1)) 0
    :: IO (AIO.IOUArray (Int, Int) Int)
  VU.forM_ [0 .. h - 1] \i -> do
    let
      loop :: Int -> Int -> IO Int
      loop j n
        | j == w = return n
        | xs A.! (i, j) == '.' = loop (j + 1) (n + 1) >>=
          \res -> AIO.writeArray yoko (i, j) res >>
          return res
        | otherwise = loop (j + 1) 0 >> return n
    loop 0 0
  tate <- AIO.newArray ((0, 0), (h - 1, w - 1)) 0
    :: IO (AIO.IOUArray (Int, Int) Int)
  VU.forM_ [0 .. w - 1] \j -> do
    let
      loop :: Int -> Int -> IO Int
      loop i n
        | i == h = return n
        | xs A.! (i, j) == '.' = loop (i + 1) (n + 1) >>=
          \res -> AIO.writeArray tate (i, j) res >>
          return res
        | otherwise = loop (i + 1) 0 >> return n
    loop 0 0
  y <- AM.freeze yoko :: IO (AU.UArray (Int, Int) Int)
  t <- AM.freeze tate :: IO (AU.UArray (Int, Int) Int)
  print . maximum
    . map (\i -> if xs A.! i == '#' then 0 else y A.! i + t A.! i - 1)
    . A.range $ ((0, 0), (h - 1, w - 1))

-------------
-- Library --
-------------

printF :: Show a => a -> IO ()
printF = putStr . show

class ReadBS a where
  readBS :: BS.ByteString -> a

instance (ReadBS a, ReadBS b) => ReadBS (a, b) where
  readBS s = (a, b) where
    [a', b'] = BS.words s
    a = readBS a'
    b = readBS b'

instance (ReadBS a, ReadBS b, ReadBS c) => ReadBS (a, b, c) where
  readBS s = (a, b, c) where
    [a', b', c'] = BS.words s
    a = readBS a'
    b = readBS b'
    c = readBS c'

-- こうした方がStringを経由しないので高速？
instance ReadBS Int where
  readBS s = case BS.readInt s of
    Just (x, _) -> x
    Nothing     -> error "readBS :: ByteString -> Int"

instance ReadBS Double where
  readBS = read . BS.unpack

instance {-# OVERLAPS #-} (ReadBS a, VG.Vector v a) => ReadBS (v a) where
  readBS s = VG.fromList . map readBS . BS.words $ s

instance {-# OVERLAPS #-} ReadBS a => ReadBS [a] where
  readBS s = map readBS . BS.words $ s

getLn :: (ReadBS a, VG.Vector v a) => Int -> IO (v a)
getLn n = VG.replicateM n get

get :: ReadBS a => IO a
get = readBS <$> BS.getLine

class Ring a where
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

repsquares :: (a -> a -> a) -> a -> Int -> a
repsquares f = loop where
  loop a n
    | n == 1 = a
    | n == 2 = f a a
    | n >= 1 && even n = loop (loop a (n `div` 2)) 2
    | n >= 1 && odd n = f a (loop a (n - 1))
    | otherwise = error "repsquares : Invalid Integer"

(^) :: Ring a => a -> Int -> a
a ^ n = repsquares (*) a n

(^^) :: Field a => a -> Int -> a
a ^^ n
  | n < 0 = recip $ a ^ (- n)
  | n == 0 = one
  | otherwise = a ^ n

instance Num a => Ring a where
  (+) = (Prelude.+)
  (*) = (Prelude.*)
  zero = 0
  negate = Prelude.negate

instance Fractional a => Field a where
  recip = Prelude.recip
  one = 1

newtype ModInt (p :: Nat) = ModInt Int deriving (Eq)

instance Show (ModInt p) where
  show (ModInt x) = show x

modint :: forall p. KnownNat p => Int -> ModInt p
modint n = ModInt $ n `mod` p
  where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)

frommodint :: ModInt p -> Integer
frommodint (ModInt n) = toInteger n

instance {-# OVERLAPS #-} (KnownNat p) => Ring (ModInt p) where
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  zero = ModInt 0
  negate (ModInt x) = ModInt $ - x `mod` p where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)

instance {-# OVERLAPS #-} (KnownNat p) => Field (ModInt p) where
  one = ModInt 1
  recip n = n ^ (p - 2) where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
