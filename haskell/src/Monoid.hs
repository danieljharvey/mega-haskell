module Monoid where

import Data.Semigroup

-- List bits

combineList :: [[a]] -> [a]
combineList (a : as) = a ++ (combineList as)

great :: [Int]
great = combineList1 [[1, 2, 3], [4, 5, 6]]

-- great == [1,2,3,4,5,6]

{-
error :: [Int]
error = combineList []
-}

ok :: [Int]
ok = combineList1 []

-- ok == []

combineList1 :: [[a]] -> [a]
combineList1 [] = []
combineList1 (a : as) = a ++ (combineList1 as)

{-
instance Semigroup [a] where
  a <> b = a ++ b

instance Monoid [a] where
  mempty = []
-}

data MyMaybe a = Yeah a | Nope

-- Sum Monoid

newtype MySum a
  = MySum
      { getMySum :: a
      }

instance (Num a) => Semigroup (MySum a) where
  MySum a <> MySum b = MySum (a + b)

instance (Num a) => Monoid (MySum a) where

  mappend = (<>)

  mempty = MySum 0

ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2

-- Product Monoid

newtype MyProduct a
  = MyProduct
      { getMyProduct :: a
      }

instance (Num a) => Semigroup (MyProduct a) where
  MyProduct a <> MyProduct b = MyProduct (a * b)

instance (Num a) => Monoid (MyProduct a) where

  mappend = (<>)

  mempty = MyProduct 1

sixtySix :: Int
sixtySix = getMyProduct $ MyProduct 11 <> MyProduct 2 <> MyProduct 3
