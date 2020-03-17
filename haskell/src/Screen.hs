{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Screen where

import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BS
import Data.Function ((&))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

-- a screen takes some props and returns some html which does shit

type StateType = Int

data HTML a
  = Div [T.Text] [HTML a]
  | StringLit T.Text
  | Button a T.Text

renderHTML :: (JSON.ToJSON a) => State a -> HTML a -> T.Text
renderHTML st (Div classNames items) =
  "<div class='" <> T.intercalate "" classNames <> "'>" <> inner <> "</div>"
  where
    inner = T.concat $ renderHTML st <$> items
renderHTML _ (StringLit s) = s
renderHTML st (Button newState title) =
  "<button onclick={"
    <> eventJS st newState
    <> "}>"
    <> title
    <> "</button>"

-- what should we run to do the update
eventJS :: (JSON.ToJSON a) => State a -> a -> T.Text
eventJS oldState i = "updateScreen(" <> json <> ")"
  where
    newState =
      oldState {info = i}
    json =
      byteStringToText $ JSON.encode newState

drawScreen :: ScreenDrawer StateType
drawScreen =
  ScreenDrawer
    ( \i ->
        Div
          ["main"]
          [ Div [] [Button (i - 1) "-", Button (i + 1) "+"],
            StringLit (T.pack $ show i)
          ]
    )

newtype ScreenDrawer s
  = ScreenDrawer {getScreenDrawer :: s -> HTML s}

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = decodeUtf8 . BS.toStrict

textToByteString :: T.Text -> BS.ByteString
textToByteString = BS.fromStrict . encodeUtf8

defaultConfig :: Config
defaultConfig = Config "*" 8000

main :: IO ()
main = do
  Warp.runSettings (makeSettings defaultConfig) application

application ::
  Wai.Application
application =
  simpleCors $
    ( \request respond ->
        requestHandler request
          >>= respond
    )

requestHandler ::
  (MonadIO m) =>
  Wai.Request ->
  m Wai.Response
requestHandler request =
  if Wai.requestMethod request == HTTP.methodPost
    then (liftIO $ Wai.getRequestBodyChunk request) >>= handlePostRequest
    else
      pure
        ( Wai.responseLBS
            HTTP.status400
            []
            "No response!"
        )

data State a
  = State
      { info :: a,
        divId :: T.Text,
        apiUrl :: T.Text
      }
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

data Output
  = Output
      { html :: T.Text,
        js :: T.Text
      }
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

-- post means plop an event in the store
handlePostRequest ::
  (MonadIO m) =>
  BS8.ByteString ->
  m Wai.Response
handlePostRequest jsonStr = do
  case JSON.decode (BS.fromStrict jsonStr) of
    Just a -> do
      -- decode the request
      let status = HTTP.status200
      let headers = []
      let body =
            JSON.encode
              ( Output
                  { html =
                      renderHTML a (getScreenDrawer drawScreen (info a)),
                    js = ""
                  }
              )
      pure (Wai.responseLBS status headers body)
    Nothing -> do
      pure
        ( Wai.responseLBS
            HTTP.status400
            []
            "No response!"
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
