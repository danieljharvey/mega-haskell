{-# LANGUAGE DerivingStrategies #-}
module Deriving where

-- in which we derive some typeclasses using fancy bullshit

data Pets
  = Dog
  | Cat
  | Gerbil
  deriving stock (Eq, Ord, Show)

-- deriving Stock means built-in typeclasses that Haskell inately understands
