module Semigroup where

import           Data.Semigroup

-- lists

firstList :: [Int]
firstList = [1,2,3]

secondList :: [Int]
secondList = [4,5,6]

thirdList :: [Int]
thirdList = firstList <> secondList

-- strings

firstString :: String
firstString = "Great"

secondString :: String
secondString = "Stuff"

thirdString :: String
thirdString = firstString <> secondString

-- MySum

newtype MySum a = MySum {
    getMySum :: a
}

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2
-- ten == 10

anotherTen :: Int
anotherTen = getMySum $ MySum 1 <> (MySum 7 <> MySum 2)
-- anotherTen == 10

-- MyProduct

newtype MyProduct a = MyProduct {
    getMyProduct :: a
}

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct a <> MyProduct b = MyProduct (a * b)

sixtySix :: Int
sixtySix = getMyProduct $ MyProduct 11 <> MyProduct 2 <> MyProduct 3
-- sixtySix = 66

anotherSixtySix :: Int
anotherSixtySix = getMyProduct $ MyProduct 11 <> (MyProduct 2 <> MyProduct 3)
-- anotherSixtySix = 66

-- MyMaybe

data MyMaybe a = Yeah a | Nope deriving (Show, Eq)

instance (Semigroup a) => Semigroup (MyMaybe a) where
  (Yeah a) <> (Yeah b) = Yeah (a <> b)
  a        <> Nope     = a
  Nope     <> b        = b

nah :: MyMaybe String
nah = Nope <> Nope
-- nah == Nope

first :: MyMaybe String
first = Yeah "Totally" <> Nope
-- first == Yeah "Totally"

second :: MyMaybe String
second = Nope <> Yeah "Great"
-- second == Yeah "Great"

both :: MyMaybe String
both = Yeah "Totally" <> Yeah "Great"
-- both = Yeah "TotallyGreat"




newtype MyString = MyString {
    getMyString :: String
}

instance Semigroup MyString where
    MyString a <> MyString b = MyString (a ++ b)

allTheThings :: String
allTheThings = getMyString $ MyString "All" <> MyString "The" <> MyString "Things"
-- allTheThings = "AllTheThings"

otherAllTheThings :: String
otherAllTheThings = getMyString $ MyString "All" <> (MyString "The" <> MyString "Things")
-- otherAllTheThings = "AllTheThings"
