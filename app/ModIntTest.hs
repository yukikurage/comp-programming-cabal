{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Proxy   as Proxy
import qualified GHC.TypeNats as TypeNats
import           Prelude      hiding (negate, recip, (*), (+), (-), (/), (^),
                               (^^))
import qualified Prelude

main :: IO ()
main = return ()

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

newtype ModInt (p :: TypeNats.Nat) = ModInt Int deriving Eq

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

instance TypeNats.KnownNat p => Field (ModInt p) where
  one = ModInt 1
  recip n = n ^ (p - 2) where
    p = fromInteger . toInteger $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p) :: Int

modint :: forall p. TypeNats.KnownNat p => Int -> ModInt p
modint n = ModInt $ n `mod` p
  where
    p = fromInteger . toInteger $
      TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)

frommodint :: ModInt p -> Integer
frommodint (ModInt n) = toInteger n
