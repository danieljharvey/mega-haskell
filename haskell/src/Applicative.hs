module Applicative where

import Data.List (concat, intersperse)
import Data.Monoid ((<>))

moreList :: Integer -> [Integer]
moreList a = [a -1, a, a + 1]

listOfLists :: [[Integer]]
listOfLists = fmap moreList [1, 2, 3]

-- listOfLists = [[0,1,2], [1,2,3], [2,3,4]]

bigList :: [Integer]
bigList = [1, 2, 3] >>= moreList

-- bigList = [0,1,2,1,2,3,2,3,4]

minusOne :: Integer -> Integer
minusOne i = i - 1

doNothing :: Integer -> Integer
doNothing = id

plusOne :: Integer -> Integer
plusOne i = i + 1

applicativeList :: [Integer]
applicativeList = [minusOne, doNothing, plusOne] <*> [1, 2, 3]

data CalcFace a = CalcFace [String] a deriving (Eq, Show)

showCalculation :: (Show a) => CalcFace a -> String
showCalculation (CalcFace names a) =
  concat parts ++ " equals " ++ show a
  where
    parts = intersperse " " names

instance Functor CalcFace where
  fmap f (CalcFace names a) = CalcFace names $ f a

one :: CalcFace Int
one = CalcFace ["1"] 1

-- showCalculation one == "1 equals 1"

two :: CalcFace Int
two = fmap (+ 1) one

-- showCalculation one == "1 equals 2". Doh.

addOne :: CalcFace (Int -> Int)
addOne = CalcFace ["add 1"] (+ 1)

instance Applicative CalcFace where

  (CalcFace operation f) <*> (CalcFace value a) =
    CalcFace newNames $ f a
    where
      newNames = value <> operation

  pure = CalcFace []

oneAddOne :: CalcFace Int
oneAddOne = addOne <*> one

oneAddOneAddOne :: CalcFace Int
oneAddOneAddOne = addOne <*> oneAddOne

instance Monad CalcFace where
  (CalcFace _ a) >>= k = k a

addThreeMonadically :: Int -> CalcFace Int
addThreeMonadically i = CalcFace ["add 3"] $ i + 3

oneAddThreeMonadically :: CalcFace Int
oneAddThreeMonadically = one >>= addThreeMonadically

oneAddThreeAddThreeMonadically :: CalcFace Int
oneAddThreeAddThreeMonadically =
  one >>= addThreeMonadically
    >>= addThreeMonadically
