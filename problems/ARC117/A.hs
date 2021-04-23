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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
  [a, b] <- get @[Int]
  putStrLn . vecToStr $ if a > b then solveA a b else if a < b then solveB a b else [1, 2 .. a] VU.++ [-1, -2 .. - b]
  return ()

solveA a b = bef VU.++ aft `VU.snoc` (- VU.sum bef - VU.sum aft)
  where
    bef = [1, 2 .. a]
    aft = [-1, -2 .. - b + 1]

solveB a b = bef VU.++ aft `VU.snoc` (- VU.sum bef - VU.sum aft)
  where
    bef = [1, 2 .. a - 1]
    aft = [-1, -2 .. - b]

vecToStr xs
  | VU.length xs == 1 = show $ VU.head xs
  | otherwise = show (VU.head xs) ++ " " ++ vecToStr (VU.tail xs)

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

instance (Readable a) => Readable [a] where
  fromBS = map fromBS . BS.split ' '

class Ring a where
  (+), (-) :: a -> a -> a
  (*) :: a -> a -> a
  zero :: a
  one :: a
  negate :: a -> a

  x - y = x + negate y
  negate x = zero - x

class (Ring a) => Field a where
  recip :: a -> a
  (/) :: a -> a -> a
  x / y = x * recip y
  recip x = one / x

infixl 8 ^

infixl 6 +

infixl 6 -

infixl 7 *

infixl 7 /

(^) :: Ring a => a -> Int -> a
a ^ n
  | n < 0 = error "^ : negative powered"
  | n == 0 = one
  | n == 2 = a * a
  | even n = (a ^ (n `div` 2)) ^ 2
  | otherwise = a * (a ^ (n `div` 2)) ^ 2

(^^) :: Field a => a -> Int -> a
a ^^ n
  | n < 0 = recip $ a ^^ (- n)
  | n == 0 = one
  | n == 2 = a * a
  | even n = (a ^^ (n `div` 2)) ^^ 2
  | otherwise = a * (a ^^ (n `div` 2)) ^^ 2

instance Num a => Ring a where
  (+) = (Prelude.+)
  (*) = (Prelude.*)
  zero = 0
  one = 1
  negate = Prelude.negate

instance Fractional a => Field a where
  recip = Prelude.recip

newtype ModInt (p :: Nat) = ModInt Int deriving (Eq)

instance Show (ModInt p) where
  show (ModInt x) = show x

modint :: forall p. KnownNat p => Int -> ModInt p
modint n = ModInt $ n `mod` p
  where
    p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)

instance {-# OVERLAPS #-} (KnownNat p) => Ring (ModInt p) where
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p
    where
      p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p
    where
      p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
  zero = ModInt 0
  one = ModInt 1
  negate (ModInt x) = ModInt $ - x `mod` p
    where
      p = fromInteger . toInteger $ natVal (Proxy :: Proxy p)
