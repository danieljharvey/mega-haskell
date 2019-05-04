module Variant where

import Prelude
import Data.Array (reverse)
import Data.Foldable (foldr)
import Data.Variant (SProxy(..), Variant, inj, match)

a :: Int
a = 1

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

loginReducer :: Login -> State -> State
loginReducer (StartLogin _ _) s
  = s { loggedIn = false, loggingIn = true }
loginReducer Logout s
  = s { loggedIn = false, loggingIn = false }
loginReducer LoginSuccess s
  = s { loggedIn = true, loggingIn = false }

countReducer :: Counting -> State -> State
countReducer Up s
  = s { value = s.value + 1 }
countReducer Down s
  = s { value = s.value - 1 }

defaultState :: State
defaultState
  = { loggedIn  : false
    , loggingIn : false
    , value     : 0
    }

-- this turns a normal Login action into a Variant Login action
liftLogin 
  :: forall v. Login 
   -> Variant (login :: Login | v)
liftLogin 
  = inj (SProxy :: SProxy "login")

-- this turns a normal Counting action into a Variant Counting action
liftCounting 
  :: forall v. Counting 
   -> Variant (counting :: Counting | v)
liftCounting 
  = inj (SProxy :: SProxy "counting")

type LiftedAction 
  = Variant (login :: Login, counting :: Counting)

-- This runs an action through whichever reducer makes sense
-- @match@ uses exhaustiveness checking so we must check for every type
process :: LiftedAction -> State -> State
process action' s =
  match
    { login:    \action -> loginReducer action s
    , counting: \action -> countReducer action s
    } action'

processAll :: State -> Array LiftedAction -> State
processAll s as
  = foldr process s (reverse as)

-- try out our Redux thing
-- see how we can use 
tryLogin :: State
tryLogin 
  = processAll defaultState actions
  where
    actions
      = [ liftLogin Logout
        , liftLogin (StartLogin "poo" "woo")
        , liftLogin LoginSuccess
        , liftCounting Up
        , liftCounting Up
        , liftCounting Down        
        ]

{-

can we get a typeclass to do this?

class LiftVariant a p where
  liftVariant :: forall v. a -> p -> Variant (p :: a | v)

instance liftVariantLogin :: LiftVariant Login (SProxy "login") where
  liftVariant a p = inj p a

-}
