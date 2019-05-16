module Variant where

import Prelude
import Data.Variant (SProxy(..), Variant, inj, match)
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref (Ref, new, read, write)
import Prim.Row (class Cons)

-- here are our first actions
data Login
  = StartLogin String String
  | Logout
  | LoginSuccess

-- here are our second actions
data Counting
  = Up
  | Down

-- an example state type
type State
  = { loggedIn  :: Boolean
    , loggingIn :: Boolean
    , value     :: Int
    }

loginReducer :: (LiftedAction -> Effect Unit) -> Login -> State -> Effect State
loginReducer _ (StartLogin _ _) s
  = pure $ s { loggedIn = false, loggingIn = true }
loginReducer _ Logout s
  = pure $ s { loggedIn = false, loggingIn = false }
loginReducer _ LoginSuccess s
  = pure $ s { loggedIn = true, loggingIn = false }

countReducer :: (LiftedAction -> Effect Unit) -> Counting -> State -> Effect State
countReducer _ Up s
  = pure $ s { value = s.value + 1 }
countReducer _ Down s
  = pure $ s { value = s.value - 1 }

defaultState :: State
defaultState
  = { loggedIn  : false
    , loggingIn : false
    , value     : 0
    }

type LiftedAction 
  = Variant (login :: Login, counting :: Counting)

-- This runs an action through whichever reducer makes sense
-- @match@ uses exhaustiveness checking so we must check for every type
myProcess 
  :: (LiftedAction -> Effect Unit) 
  -> State 
  -> LiftedAction 
  -> Effect State
myProcess dispatch s action' =
  match
    { login:    \action -> loginReducer dispatch action s
    , counting: \action -> countReducer dispatch action s
    } action'


{-
processAll 
  :: forall actionType
   . actionType -> Effect Unit
  -> State
  -> Array actionType
   -> Effect State
processAll myDispatch s as
  = foldM (process myDispatch) s (reverse as)
  -}

type RunReducers actionType stateType
  = (actionType -> Effect Unit)
  -> stateType
  -> actionType
  -> Effect stateType

type Listeners stateType
  = Array (stateType -> Effect Unit)

data Store actionType stateType
  = Store { dispatch :: actionType -> Effect Unit
          , getState :: Effect stateType
          }

createStore 
  :: forall stateType actionType
   . stateType
  -> Listeners stateType
  -> RunReducers actionType stateType
  -> Effect (Store actionType stateType)
createStore state listeners reducers = do
  stateRef <- new state

  pure $ Store { dispatch: (update stateRef listeners reducers)
               , getState: (getState stateRef)
               }
  
update 
  :: forall stateType actionType
   . Ref stateType
  -> Listeners stateType
  -> RunReducers actionType stateType
  -> actionType
  -> Effect Unit
update stateRef listeners reducers action = do
  oldState <- read stateRef
  newState <- reducers (\_ -> pure unit) oldState action
  _ <- traverse (\f -> f newState) listeners
  write newState stateRef

getState
  :: forall stateType 
   . Ref stateType
  -> Effect stateType
getState stateRef = read stateRef

createAndUse :: Effect Unit
createAndUse = do 
  (Store store) <- createStore defaultState [ listener ] myProcess
  store.dispatch (liftAction' (StartLogin "poo" "woo"))

listener :: State -> Effect Unit
listener = logShow

{-
-- try out our Redux thing
-- see how we can use 
tryLogin :: Effect State
tryLogin 
  = processAll defaultState actions
  where
    actions
      = [ liftAction' Logout
        , liftAction' (StartLogin "poo" "woo")
        , liftAction' LoginSuccess
        , liftAction' Up
        , liftAction' Up
        , liftAction' Down        
        ]
      -}

class HasLabel a (p :: Symbol) | a -> p 

instance hasLabelLogin :: HasLabel Login "login" 
instance hasLabelCounting :: HasLabel Counting "counting"

liftAction' 
  :: forall label action dunno r. Cons label action dunno r 
   => HasLabel action label 
   => IsSymbol label 
   => action 
   -> Variant r
liftAction'  
  = inj (SProxy :: SProxy label) 
