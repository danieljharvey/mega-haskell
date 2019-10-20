module Newtypes where

import Data.Maybe (fromMaybe)

newtype Dog a = Dog {getDog :: a} deriving (Show)

frank :: Dog String
frank = Dog "Frank"

-- frank == Dog "Frank"

name :: String
name = getDog frank

-- name == "Frank"

itsTheSame :: Bool
itsTheSame = "Frank" == getDog (Dog "Frank")

newtype Cat a = Cat {getCat :: a} deriving (Show)

mimas :: Cat String
mimas = Cat "Mimas"

-- mimas == Cat "Mimas"

data Hand = Hand

class Pettable a where
  (<>|<>) :: Hand -> a -> String

instance (Show a) => Pettable (Dog a) where
  Hand <>|<> (Dog a) = show a ++ " woofs or something"

instance (Show a) => Pettable (Cat a) where
  Hand <>|<> (Cat a) = show a ++ " miaows or similar"

calculateSalaryBad :: Int -> Int
calculateSalaryBad months = months * 1000

calculateSalaryBetter :: (Num a, Show a, Ord a) => a -> Maybe a
calculateSalaryBetter i =
  if i < 0
    then Nothing
    else Just (i * 1000)

newtype PositiveNum a = PositiveNum {getPositiveNum :: a} deriving (Eq, Show)

makePositiveNum :: (Num a, Ord a) => a -> Maybe (PositiveNum a)
makePositiveNum i
  | i < 0 = Nothing
  | otherwise = Just (PositiveNum i)

zero :: (Num a) => PositiveNum a
zero = PositiveNum 0

months :: (Num a, Ord a) => a -> PositiveNum a
months i = fromMaybe zero (makePositiveNum i)

calculateSalary :: (Num a) => PositiveNum a -> a
calculateSalary months = 1000 * (getPositiveNum months)

instance Functor PositiveNum where
  fmap f (PositiveNum i) = PositiveNum (f i)

calculateSalary2 :: (Num a) => PositiveNum a -> PositiveNum a
calculateSalary2 = fmap (* 1000)

calculateSalary3 :: (Num a) => PositiveNum a -> a
calculateSalary3 i = getPositiveNum $ fmap (* 1000) i

yes :: PositiveNum Int
yes = months 12

-- yes = PositiveNum 12

nope :: PositiveNum Int
nope = months (-12)
-- nope = PositiveNum 0
