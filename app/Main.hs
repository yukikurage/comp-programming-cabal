{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O2 #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Array.IArray as A
import qualified Data.Array.IO as AIO
import qualified Data.Array.MArray as AM
import qualified Data.Array.ST as AST
import qualified Data.Array.Unboxed as AU
import qualified Data.Attoparsec.ByteString as PBS
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Heap as Heap
import Data.IORef
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Primitive.MutVar
import Data.Proxy
import Data.STRef
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VAM
import Data.Vector.Algorithms.Search
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace
import GHC.TypeNats
import System.IO
import Prelude hiding
  ( negate,
    recip,
    (*),
    (+),
    (-),
    (/),
    (^),
    (^^),
  )
import qualified Prelude

----------
-- Main --
----------

main :: IO ()
main = do
  return ()

-------------
-- Library --
-------------

printF :: Show a => a -> IO ()
printF = putStr . show

class Readable a where
  fromBS :: BS.ByteString -> a

get :: Readable a => IO a
get = fromBS <$> BS.getLine

getLn :: (Readable a, VU.Unbox a) => Int -> IO (VU.Vector a)
getLn n = VU.replicateM n get

instance Readable Int where
  fromBS =
    fst . fromMaybe do error "Error : fromBS @Int"
      . BS.readInt

instance Readable Double where
  fromBS = read . BS.unpack

instance KnownNat p => Readable (ModInt p) where
  fromBS = modint . fromBS

instance (Readable a, Readable b) => Readable (a, b) where
  fromBS bs = (fromBS x0, fromBS x1)
    where
      [x0, x1] = BS.split ' ' bs

instance (Readable a, Readable b, Readable c) => Readable (a, b, c) where
  fromBS bs = (fromBS x0, fromBS x1, fromBS x2)
    where
      [x0, x1, x2] = BS.split ' ' bs

instance (Readable a, VU.Unbox a) => Readable (VU.Vector a) where
  fromBS = VU.fromList . map fromBS . BS.split ' '

instance (Readable a) => Readable (V.Vector a) where
  fromBS = V.fromList . map fromBS . BS.split ' '

instance (Readable a) => Readable [a] where
  fromBS = map fromBS . BS.split ' '

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
repsquares f = loop
  where
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
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p
    where
      p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p
    where
      p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  zero = ModInt 0
  negate (ModInt x) = ModInt $ - x `mod` p
    where
      p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)

instance {-# OVERLAPS #-} (KnownNat p) => Field (ModInt p) where
  one = ModInt 1
  recip n = n ^ (p - 2)
    where
      p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
