module GenericJson where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)

type Error
  = { code :: Int
    , title :: String
    }

type Reply
  = { name :: String }

-- our Reducer data type
data Login
  = StartLogin String String
  | LoginReply (Either Error Reply)
  | Logout

-- by deriving Generic we can use automatic functions for encodeJson and
-- decodeJson
derive instance genericLogin :: Generic Login _

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson = genericEncodeJson

instance decodeJsonLogin :: DecodeJson Login where
  decodeJson = genericDecodeJson

-- let's give it a spin
a :: Json
a = encodeJson (StartLogin "poo" "woo")

b :: Json
b = encodeJson (LoginReply (Right { name: "Dr Dog" }))

c :: Either String Login
c = decodeJson a

d :: Either String Login
d = decodeJson b

-- nice
data Counting
  = Up
  | Down

derive instance genericCounting :: Generic Counting _

instance encodeJsonCounting :: EncodeJson Counting where
  encodeJson = genericEncodeJson

instance decodeJsonCounting :: DecodeJson Counting where
  decodeJson = genericDecodeJson

-- an example state type
type State
  = { loggedIn :: Boolean
    , loggingIn :: Boolean
    , value :: Int
    }

loginReducer :: State -> Login -> State
loginReducer s (StartLogin _ _) = s { loggedIn = false, loggingIn = true }

loginReducer s Logout = s { loggedIn = false, loggingIn = false }

loginReducer s (LoginReply (Left _)) = s { loggedIn = false, loggingIn = false }

loginReducer s (LoginReply (Right _)) = s { loggedIn = true, loggingIn = false }

countReducer :: State -> Counting -> State
countReducer s Up = s { value = s.value + 1 }

countReducer s Down = s { value = s.value - 1 }

defaultState :: State
defaultState =
  { loggedIn: false
  , loggingIn: false
  , value: 0
  }

-- if we can match this, do the action
tryReducer ::
  forall a s.
  DecodeJson a =>
  Reducer a s ->
  s ->
  Json ->
  s
tryReducer f state json = case decodeJson json of
  Right action -> f state action
  _ -> state

e :: State
e = tryReducer loginReducer defaultState (encodeJson (StartLogin "poo" "woo"))

g :: State
g = tryReducer loginReducer defaultState (encodeJson Up)

h :: State
h = tryReducer countReducer defaultState (encodeJson Up)

-- record of reducers
-- can this type be generalised to a record of (s -> a -> s) functions?
reducers ::
  { login :: Reducer Login State
  , count :: Reducer Counting State
  }
reducers =
  { login: loginReducer
  , count: countReducer
  }

type Reducer a s
  = s -> a -> s

data RunReducer
  = RunReducer

instance runReducerFold ::
  DecodeJson a =>
  Folding RunReducer (Tuple s Json) (s -> a -> s) (Tuple s Json) where
  folding RunReducer (Tuple state json) reducer = Tuple newState json
    where
    newState = tryReducer reducer state json

-- can we abstract the reducers out of this and pass them in too?
runReducers ::
  forall reducer state json.
  HFoldl RunReducer (Tuple state json) reducer (Tuple state json) =>
  reducer ->
  state ->
  json ->
  state
runReducers reducers' state json = fst $ hfoldl RunReducer (Tuple state json) reducers'
