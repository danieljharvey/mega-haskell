{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Semilattice where

-- what is a semilattice?
import           Data.Proxy
import           Data.Semilattice.Join
import           Data.Semilattice.Lower
import           Data.Semilattice.Upper
import qualified Data.Set               as Data.Set
import           GHC.TypeLits           (Symbol)

data Colour
  = Black
  | DarkGrey
  | Grey
  | LightGrey
  | White
  deriving stock (Eq, Ord, Show)
  deriving anyclass (Lower, Upper)

instance Bounded Colour where
  minBound = Black
  maxBound = White

--
instance Join Colour where
  a \/ b = if a < b then b else a

-- They are idempotent -- the thing with itself == itself
a = Black \/ Black
-- a == Black

-- They are commutative - which means the order doesn't change the result
b = DarkGrey \/ Black
-- b == DarkGrey
c = Black \/ DarkGrey
-- c == DarkGrey

-- They are associative - so grouping them shouldn't change the result
d = DarkGrey \/ (Grey \/ LightGrey)
-- d == LightGrey
e = (DarkGrey \/ Grey) \/ LightGrey
-- e == LightGrey

-- If they are Lower bounded, then any X combined with the lower == X
f = LightGrey \/ Black
-- f == LightGrey

-- If they are Upper bounded, then any X combined with upper == upper
g = LightGrey \/ White
-- g == White

-------- a more interesting example

newtype Nat
  = Nat { unNat :: Integer }
  deriving (Eq, Ord, Show)

makeNat :: Integer -> Nat
makeNat i = if i < 0
            then Nat 0
            else Nat i

data ActionType a
  = NoOp
  | Update a
  deriving (Eq, Show)

data Action a
  = Action { actionType :: ActionType a
           , index      :: Nat
           }
  deriving (Eq, Show)

instance Lower (Action a) where
  lowerBound = Action NoOp (makeNat 0)

instance (Eq a) => Ord (Action a) where
  a <= b = index a <= index b

type ActionList a = Data.Set.Set (Action a)

action1 = Data.Set.singleton (Action NoOp (makeNat 1))

action2 = Data.Set.singleton (Action actionType (makeNat 2))
  where
    actionType
      = Update "dog"

action3 = Data.Set.singleton (Action (Update "log") (makeNat 3))

action4 = Data.Set.singleton (Action (Update "bog") (makeNat 4))

a' :: ActionList String
a' = action1 \/ action2

b' :: ActionList String
b' = action2 \/ action1 \/ action4 \/ action3

c' = a' == b'

foldAction :: ActionType a -> a -> a
foldAction NoOp a       = a
foldAction (Update a) _ = a

runActionList :: a -> ActionList a -> a
runActionList initial items
  = foldl foldAction' initial items
  where
    foldAction' a action
      = foldAction (actionType action) a

d' = runActionList "" a'

e' = runActionList "" b'


