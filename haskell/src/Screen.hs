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
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

-- a screen takes some props and returns some html which does shit

data HTML
  = Div [HTML]
  | StringLit T.Text
  | Button T.Text

renderHTML :: HTML -> T.Text
renderHTML (Div items) =
  "<div>" <> inner <> "</div>"
  where
    inner = T.concat $ renderHTML <$> items
renderHTML (StringLit s) = s
renderHTML (Button title) = "<button>" <> title <> "</button>"

drawScreen :: Int -> HTML
drawScreen i =
  Div
    [ Div [Button "-", Button "+"],
      StringLit (T.pack $ show i)
    ]

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
                      renderHTML (drawScreen (info a)),
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

}
-}
