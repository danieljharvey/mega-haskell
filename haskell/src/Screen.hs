{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Screen where

import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BS
import Data.Function ((&))
import Data.Hashable
import Data.IORef
import Data.Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

-- a screen takes some props and returns some html which does shit

type StateType = Int

data HTML action
  = Div [T.Text] [HTML action]
  | StringLit T.Text
  | Button action T.Text

renderHTML :: (JSON.ToJSON action, JSON.ToJSON state, Hashable state) => state -> HTML action -> T.Text
renderHTML st (Div classNames items) =
  "<div class='" <> T.intercalate "" classNames <> "'>" <> inner <> "</div>"
  where
    inner = T.concat $ renderHTML st <$> items
renderHTML _ (StringLit s) = s
renderHTML st (Button action' title) =
  "<button onclick={"
    <> eventJS (getStateHash st) action'
    <> "}>"
    <> title
    <> "</button>"

-- what should we run to do the update
eventJS :: (JSON.ToJSON action) => HashKey -> action -> T.Text
eventJS stateHash action' = escapeT $ "function go() { updateScreen(" <> hashText <> ", " <> actionJson <> ")}"
  where
    hashText =
      T.pack (show stateHash)
    actionJson =
      byteStringToText $ JSON.encode action'

drawScreen :: StateType -> HTML Action
drawScreen =
  ( \st ->
      Div
        ["main"]
        [ Div [] [Button Decrease "-", Button Increase "+"],
          StringLit (T.pack $ show st)
        ]
  )

escape :: String -> String
escape "" = ""
escape ('\"' : t) = "\"" <> escape t
escape (x : xs) = x : escape xs

escapeT :: T.Text -> T.Text
escapeT = T.pack . escape . T.unpack

data Action
  = NoOp
  | Increase
  | Decrease
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)

type Reducer state action = state -> action -> state

reducer :: Reducer StateType Action
reducer state action' = case action' of
  NoOp -> state
  Increase -> state + 1
  Decrease -> state - 1

type StateMap = Map HashKey JSON.Value

newtype HashKey = HashKey {getHashKey :: Int}
  deriving newtype (Eq, Ord, Show, JSON.ToJSON, JSON.FromJSON)

---
--
getStateHash :: (Hashable a, JSON.ToJSON a) => a -> HashKey
getStateHash = HashKey . Data.Hashable.hash . JSON.encode

storeState :: (Hashable a, JSON.ToJSON a) => StateMap -> a -> StateMap
storeState store a = insert (getStateHash a) (JSON.toJSON a) store

fetchFromStore :: (JSON.FromJSON a) => StateMap -> HashKey -> Maybe a
fetchFromStore store key = Data.Map.lookup key store >>= resultToJSON . JSON.fromJSON
  where
    resultToJSON a = case a of
      JSON.Success a' -> Just a'
      _ -> Nothing

---

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = decodeUtf8 . BS.toStrict

textToByteString :: T.Text -> BS.ByteString
textToByteString = BS.fromStrict . encodeUtf8

defaultConfig :: Config
defaultConfig = Config "*" 8000

main :: IO ()
main = do
  stateMap <- newIORef mempty
  Warp.runSettings (makeSettings defaultConfig) (application stateMap)

application ::
  IORef StateMap ->
  Wai.Application
application stateMap =
  simpleCors $
    ( \request respond ->
        requestHandler stateMap request
          >>= respond
    )

requestHandler ::
  (MonadIO m) =>
  IORef StateMap ->
  Wai.Request ->
  m Wai.Response
requestHandler stateMap request =
  if Wai.requestMethod request == HTTP.methodPost
    then
      (liftIO $ Wai.getRequestBodyChunk request)
        >>= (handlePostRequest stateMap)
    else
      pure
        ( Wai.responseLBS
            HTTP.status400
            []
            "Post responses only!"
        )

data Output
  = Output
      { html :: T.Text,
        js :: T.Text
      }
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

data Incoming action
  = Incoming {hashKey :: HashKey, action :: action}
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

-- post means plop an event in the store
handlePostRequest ::
  (MonadIO m) =>
  IORef StateMap ->
  BS8.ByteString ->
  m Wai.Response
handlePostRequest stateMap jsonStr = do
  case JSON.decode (BS.fromStrict jsonStr) of
    Just incoming -> do
      state <- liftIO $ readIORef stateMap
      -- decode the request
      let status = HTTP.status200
      let headers = []
      let oldState = fromMaybe 0 (fetchFromStore state (hashKey incoming))
      let newState = reducer oldState (action incoming)
      let newStateMap = storeState state newState
      liftIO (print oldState)
      liftIO (print (action incoming))
      liftIO (print newState)
      liftIO (print (length newStateMap))
      liftIO $ writeIORef stateMap newStateMap
      let body =
            JSON.encode
              ( Output
                  { html =
                      renderHTML newState (drawScreen newState),
                    js = ""
                  }
              )
      pure (Wai.responseLBS status headers body)
    Nothing -> do
      pure
        ( Wai.responseLBS
            HTTP.status400
            []
            "Could not decode response!"
        )

data Config
  = Config
      { configHost :: Warp.HostPreference,
        configPort :: Warp.Port
      }
  deriving (Eq, Show)

makeSettings :: Config -> Warp.Settings
makeSettings config =
  Warp.defaultSettings & Warp.setHost (configHost config)
    & Warp.setPort (configPort config)
{-

function update(state, url) {
  fetch(
}
-}
