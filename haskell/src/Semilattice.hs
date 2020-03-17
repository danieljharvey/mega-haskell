{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Semilattice where

-- what is a semilattice?
import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Upper
import qualified Data.Set as Data.Set
import Data.Time.Clock.POSIX (getPOSIXTime)

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

-- If they are Lower bounded, then any X combined with the lower == X
f :: Colour
f = LightGrey \/ Black

-- f == LightGrey

-- If they are Upper bounded, then any X combined with upper == upper
g :: Colour
g = LightGrey \/ White

-- g == White

-------- a more interesting example

data ActionType a
  = NoOp
  | OnChange a
  | OnBlur
  | OnClick
  | OnFocus
  deriving (Eq, Show)

newtype Timestamp
  = Timestamp {getTimestamp :: Integer}

data Action a
  = Action
      { actionType :: ActionType a,
        timestamp :: Integer
      }
  deriving (Eq, Show)

instance Lower (Action a) where
  lowerBound = Action NoOp 0

-- the events are ordered by the timestamp
instance (Eq a) => Ord (Action a) where
  a <= b = timestamp a <= timestamp b

type ActionList a = Data.Set.Set (Action a)

action1 :: ActionList a
action1 = createAction 1 NoOp

action2 :: ActionList String
action2 = createAction 2 (OnChange "dog")

action3 :: ActionList String
action3 = createAction 3 (OnChange "log")

action4 :: ActionList String
action4 = createAction 4 (OnChange "bog")

getCurrentTimestamp :: IO Integer
getCurrentTimestamp = (round . (* 1000)) <$> getPOSIXTime

createIOAction :: ActionType a -> IO (ActionList a)
createIOAction a = do
  timestamp' <- getCurrentTimestamp
  pure $ Data.Set.singleton (Action a timestamp')

createAction :: Integer -> ActionType a -> ActionList a
createAction timestamp' a = Data.Set.singleton (Action a timestamp')

a' :: ActionList String
a' = action1 \/ action2

b' :: ActionList String
b' = action2 \/ action1 \/ action4 \/ action3

c' :: Bool
c' = a' == b'

foldAction :: ActionType a -> a -> a
foldAction NoOp a = a
foldAction (OnChange a) _ = a
foldAction OnBlur a = a
foldAction OnClick a = a
foldAction OnFocus a = a

runActionList :: a -> ActionList a -> a
runActionList initial items =
  foldl foldAction' initial items
  where
    foldAction' a action =
      foldAction (actionType action) a

d' :: String
d' = runActionList "" a'

e' :: String
e' = runActionList "" b'
