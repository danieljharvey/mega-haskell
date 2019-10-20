module Bifunctor where

import Data.Bifunctor

data Things a b = This a | That b
  deriving (Eq, Show)

instance Functor (Things a) where
  fmap f (That b) = That (f b)
  fmap _ (This a) = This a

thisEgg :: Things String Int
thisEgg = This "Egg"

thatNumber :: Things String Int
thatNumber = That 68

addOne :: Int -> Int
addOne i = i + 1

biggerNumber :: Things String Int
biggerNumber = fmap addOne (That 68)

-- biggerNumber == That 69

eat :: String -> String
eat s = "The " ++ s ++ " was delicious!"

{-
doesntWork :: Things String Int
doesntWork = fmap eat thisEgg
-- Couldn't match type ‘[Char]’ with ‘Int’
-}

doesNothing :: Things String Int
doesNothing = fmap addOne thisEgg

-- doesNothing = This "Egg"

-- let's make a bifunctor

{-
class Bifunctor (p :: * -> * -> *) where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  Data.Bifunctor.first :: (a -> b) -> p a c -> p b c
  Data.Bifunctor.second :: (b -> c) -> p a b -> p a c
  {-# MINIMAL bimap | first, second #-}
-}

instance Bifunctor Things where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)

delicious :: Things String Int
delicious = first eat (This "Egg")

-- delicious = This "The Egg was delicious!"

doesWork :: Things String Int
doesWork = second addOne thatNumber

-- doesWork == That 69

twoThings :: (Int, String)
twoThings = (100, "Dogs")

myConst :: a -> b -> a
myConst a _ = a

oneBestSeller :: (Int, String)
oneBestSeller = bimap (+ 1) (myConst "Dalmations") twoThings
