{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Redux where

import Data.Data

-- we're going to have a bunch of different sum types
-- and want to listen to some and ignore others
-- can we?

-- here are our first actions
data Login
  = StartLogin String String
  | Logout
  | LoginSuccess
  deriving (Show)

-- here are our second actions
data Counting
  = Up
  | Down
  deriving (Show)

-- an example state type
data State
  = State
      { loggedIn :: Bool,
        loggingIn :: Bool,
        value :: Int
      }
  deriving (Show)

-- is our mystery type a the same as known?
containsType ::
  forall known a.
  (Typeable a, Typeable known) =>
  a ->
  Bool
containsType mystery =
  typeOf mystery == typeOf (undefined :: known)

-- this one is
y :: Bool
y = containsType @Login Logout

-- but this one isn't
n :: Bool
n = containsType @Login Down

-- can we recover our mystery type despite not knowing what it is?
showIfMatches ::
  forall known a.
  (Typeable a, Typeable known, Show a) =>
  a ->
  Maybe String
showIfMatches mystery =
  if containsType @known mystery
    then case cast mystery of
      Just (val :: a) -> Just (show val)
      _ -> Nothing
    else Nothing

-- Just "Logout"
b :: Maybe String
b = showIfMatches @Login Logout

-- Nothing
c :: Maybe String
c = showIfMatches @Login Down

processIfMatching ::
  forall known a.
  (Typeable a, Typeable known) =>
  (known -> State -> State) ->
  a ->
  State ->
  State
processIfMatching f a s =
  if containsType @known a
    then doAction
    else s
  where
    doAction =
      case cast a of
        Just (action :: known) -> f action s
        _ -> s

loginReducer :: Login -> State -> State
loginReducer (StartLogin _ _) s =
  s {loggedIn = False, loggingIn = True}
loginReducer Logout s =
  s {loggedIn = False, loggingIn = False}
loginReducer LoginSuccess s =
  s {loggedIn = True, loggingIn = False}

countReducer :: Counting -> State -> State
countReducer Up s =
  s {value = (value s) + 1}
countReducer Down s =
  s {value = (value s) - 1}

defaultState :: State
defaultState =
  State
    { loggedIn = False,
      loggingIn = False,
      value = 0
    }

d :: State
d = processIfMatching loginReducer (StartLogin "paul" "imissbarry") defaultState

e :: State
e = processIfMatching countReducer Up defaultState
-- how do I express the big list of reducers
