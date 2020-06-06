{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Path where

import Data.Set

data Node = Node String

user :: Node
user = Node "User"

pet :: Node
pet = Node "Pet"

horse :: Node
horse = Node "Horse"

data Edge a b
  = Edge a b String

type Edges = Set (Edge Node Node)
