module Variant where

import Prelude
import Data.Variant (SProxy(..), Variant, inj, match)
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref (Ref, new, read, write)
import Effect.Timer as T
import Prim.Row (class Cons)

-- here are our first actions
data Login
  = StartLogin String String
  | Logout
  | LoginSuccess

instance hasLabelLogin :: HasLabel Login "login"

-- here are our second actions
data Counting
  = Up
  | Down

instance hasLabelCounting :: HasLabel Counting "counting"

-- an example state type
type State
  = { loggedIn :: Boolean
    , loggingIn :: Boolean
    , value :: Int
    }

loginReducer ::
  Reducer LiftedAction Login State
loginReducer dispatch (StartLogin _ _) s = do
  _ <-
    T.setTimeout 100 do
      dispatch (liftAction' LoginSuccess)
  pure $ s { loggedIn = false, loggingIn = true }

loginReducer _ Logout s = pure $ s { loggedIn = false, loggingIn = false }

loginReducer _ LoginSuccess s = pure $ s { loggedIn = true, loggingIn = false }

countReducer ::
  Reducer LiftedAction Counting State
countReducer _ Up s = pure $ s { value = s.value + 1 }

countReducer _ Down s = pure $ s { value = s.value - 1 }

defaultState :: State
defaultState =
  { loggedIn: false
  , loggingIn: false
  , value: 0
  }

type LiftedAction
  = Variant ( login :: Login, counting :: Counting )

-- This runs an action through whichever reducer makes sense
-- @match@ uses exhaustiveness checking so we must check for every type
myProcess ::
  CombinedReducer LiftedAction State
myProcess dispatch s action' =
  match
    { login: \action -> loginReducer dispatch action s
    , counting: \action -> countReducer dispatch action s
    }
    action'

createAndUse :: Effect Unit
createAndUse = do
  (Store store) <- createStore defaultState [ listener ] myProcess
  store.dispatch (liftAction' (StartLogin "poo" "woo"))

listener :: State -> Effect Unit
listener = logShow

type Reducer allActionType actionType stateType
  = (allActionType -> Effect Unit) ->
    actionType ->
    stateType ->
    Effect stateType

type CombinedReducer actionType stateType
  = (actionType -> Effect Unit) ->
    stateType ->
    actionType ->
    Effect stateType

type Listeners stateType
  = Array (stateType -> Effect Unit)

type Dispatcher actionType
  = actionType -> Effect Unit

data Store actionType stateType
  = Store
    { dispatch :: Dispatcher actionType
    , getState :: Effect stateType
    }

class HasLabel a (p :: Symbol) | a -> p

createStore ::
  forall stateType actionType.
  stateType ->
  Listeners stateType ->
  CombinedReducer actionType stateType ->
  Effect (Store actionType stateType)
createStore state listeners reducers = do
  stateRef <- new state
  pure
    $ Store
        { dispatch: (update stateRef listeners reducers)
        , getState: (getState stateRef)
        }

update ::
  forall stateType actionType.
  Ref stateType ->
  Listeners stateType ->
  CombinedReducer actionType stateType ->
  actionType ->
  Effect Unit
update stateRef listeners reducers action = do
  -- read current state
  oldState <- read stateRef
  -- create a dispatcher
  let
    dispatch = update stateRef listeners reducers
  -- calculate new state
  newState <- reducers dispatch oldState action
  -- announce new state to listeners
  _ <- traverse (\f -> f newState) listeners
  -- save new state
  write newState stateRef

getState ::
  forall stateType.
  Ref stateType ->
  Effect stateType
getState stateRef = read stateRef

liftAction' ::
  forall label action dunno r.
  Cons label action dunno r =>
  HasLabel action label =>
  IsSymbol label =>
  action ->
  Variant r
liftAction' = inj (SProxy :: SProxy label)
