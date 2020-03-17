module Foldable where

added :: Int
added = foldr (\a b -> a + b) 0 [1, 2, 3, 4]

-- added = 10

maxNo :: Int
maxNo = foldr (\a b -> if a > b then a else b) 0 [1, 2, 3, 4]

-- maxNo = 4

-- let's make a new type
newtype MySum a = MySum {getMySum :: a}

instance (Num a) => Semigroup (MySum a) where
  MySum a <> MySum b = MySum (a + b)

instance (Num a) => Monoid (MySum a) where

  mappend = (<>)

  mempty = MySum 0

addTwo :: Int
addTwo = getMySum $ foldMap MySum [1, 2, 3, 4]

-- addTwo = 10

-- let's make a new type
newtype MyAll = MyAll {getMyAll :: Bool}

instance Semigroup MyAll where
  MyAll a <> MyAll b = MyAll (a && b)

instance Monoid MyAll where

  mappend = (<>)

  mempty = MyAll True

allOfThem :: Bool
allOfThem = getMyAll $ foldMap MyAll [True, True, True]

-- allOfThem == True

notAll :: Bool
notAll = getMyAll $ foldMap MyAll [False, True, True]

-- notAll == False

-- let's make a new type
newtype MyProduct a = MyProduct {getMyProduct :: a}

instance (Num a) => Semigroup (MyProduct a) where
  MyProduct a <> MyProduct b = MyProduct (a * b)

instance (Num a) => Monoid (MyProduct a) where

  mappend = (<>)

  mempty = MyProduct 1

twentyFour :: Int
twentyFour = getMyProduct $ foldMap MyProduct [1, 2, 3, 4]
-- twentyFour = 24
