{-
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
-}

{-
data DisjointSetM m = DSet
  {dsParents :: VUM.MVector m Int, dsDepths ::VUM.MVector m Int}
newDSet :: Prim.PrimMonad m => Int -> m (DisjointSetM (Prim.PrimState m))
newDSet n = DSet <$> VU.thaw (VU.generate n id) <*> VUM.replicate n 1

-- | ルートを調べる時につなぎ直す
root :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> m Int
root ds i = VUM.read (dsParents ds) i >>= \p -> if p == i
  then return i
  else root ds p >>= \r -> VUM.write (dsParents ds) i r >> return r

union :: Prim.PrimMonad m =>
  DisjointSetM (Prim.PrimState m) -> Int -> Int -> m ()
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
  DisjointSetM (Prim.PrimState m) -> Int -> Int -> m Bool
find ds i j = (==) <$> root ds i <*> root ds j
-}


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
