module Contravariant where

import Data.Functor.Contravariant

newtype Preddy a = Preddy {getPreddy :: a -> Bool}

threePred :: Int -> Bool
threePred i = i > 3

overThree :: Preddy Int
overThree = Preddy threePred

isOverThree :: Int -> Bool
isOverThree = getPreddy overThree

-- isOverThree 2 == False
-- isOverThree 4 == True

instance Contravariant Preddy where
  contramap f (Preddy p) = Preddy (p . f)

nameLength :: String -> Int
nameLength "" = 0
nameLength (x : xs) = 1 + nameLength xs

nameLengthOverThree :: Preddy String
nameLengthOverThree = contramap nameLength overThree

nameIsOverThree :: String -> Bool
nameIsOverThree = getPreddy nameLengthOverThree

-- nameIsOverThree "Lou" == False
-- nameIsOverThree "Doug" == True

data Person = Person {name :: String, age :: Int} deriving (Show)

steve :: Person
steve = Person {name = "Steve", age = 100}

lou :: Person
lou = Person {name = "Lou", age = 69}

personTooLong :: Person -> Bool
personTooLong = getPreddy personPreddy
  where
    personPreddy = contramap (nameLength . name) overThree
-- personTooLong steve == True
-- personTooLong lou == False
