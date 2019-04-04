{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Deriving where

import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy
import Data.Coerce

-- in which we derive some typeclasses using fancy bullshit

data Pet
  = Dog
  | Cat
  | Gerbil
  deriving stock (Eq, Ord, Show, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON)
  deriving (IsGoodBoy) via (PetWrapper)
-- deriving Stock means built-in typeclasses that Haskell inately understands
-- deriving anyclass uses the default instance
-- deriving via let's us steal an instance from something with the same underlying value

a :: Bool
a = Dog == Cat
-- a == False

b :: Bool
b = Dog < Cat
-- b == True

c :: String
c = show Gerbil
-- c == "Gerbil"

d :: Maybe Pet
d = decode "\"Dog\""
-- d = Just Dog

e :: ByteString
e = encode Cat
-- e == "\"Cat\""

-- let's make a wrapper that steals all of the instances from the thing it's wrapping
newtype PetWrapper
  = PetWrapper { getPet :: Pet }
    deriving stock (Generic)
    deriving newtype (Eq, Ord, Show, Bounded, Enum, ToJSON, FromJSON)

class IsGoodBoy a where
  isGoodBoy :: a -> Bool

instance IsGoodBoy PetWrapper where
  isGoodBoy a = (coerce a == Dog)

-- how do we know they are the same thing?
changeTo :: Pet -> PetWrapper
changeTo = coerce

changeFrom :: PetWrapper -> Pet
changeFrom = coerce
-- great!


-- examples?


-- let's derive more interesting behaviours

data Wrapper a
  = Thing a
  | TwoThings (a, a)
  deriving stock (Show, Foldable, Functor, Traversable)

f :: Wrapper String
f = show <$> TwoThings (10,100)
-- f == TwoThings ("10", "100")

g :: Int
g = Prelude.foldr (+) 0 $ TwoThings (1, 2)
-- g == 3

h :: Maybe (Wrapper Int)
h = sequence (TwoThings (Just 1, Just 10))
-- h == Just (TwoThings (1, 10))




