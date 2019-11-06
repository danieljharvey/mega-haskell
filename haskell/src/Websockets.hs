{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Websockets where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar,
                                     readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Char          (isPunctuation, isSpace)
import           Data.Coerce
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import qualified Data.Aeson         as JSON

import           GHC.Generics
import qualified Network.WebSockets as WS

type Client = (ClientName, WS.Connection)

newtype ClientName
  = ClientName { getClientName :: Text }
  deriving (Eq, Ord, Show)

data ServerState =
  ServerState
    { clients :: [Client]
    , events  :: M.Map Int Event
    }

newtype Event
  = Event { getEvent :: Text }
  deriving (Eq, Ord, Show)


data BasicUser
  = BasicUser
      { firstName :: Text
      , surname   :: Text
      }
    deriving (Eq, Ord, Show, Generic, JSON.FromJSON)



nextKey :: M.Map Int Event -> Int
nextKey
  = (+1)
  . (fromMaybe 0)
  . listToMaybe
  . reverse
  . M.keys

appendEvent :: Event -> ServerState -> ServerState
appendEvent event serverState
  = serverState { events = M.insert key event (events serverState) }
  where
    key
      = nextKey (events serverState)

newServerState :: ServerState
newServerState = ServerState [] mempty

numClients :: ServerState -> Int
numClients = length . clients

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst) . clients

addClient :: Client -> ServerState -> ServerState
addClient client serverState
  = serverState { clients = client : (clients serverState) }

removeClient :: Client -> ServerState -> ServerState
removeClient client serverState
  = serverState
      { clients = filter ((/= fst client) . fst) (clients serverState) }

broadcast :: Text -> ServerState -> IO ()
broadcast message serverState = do
    T.putStrLn message
    forM_ (clients serverState) $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients' <- readMVar state
    case msg of
      _   | not (prefix `T.isPrefixOf` msg) ->
              WS.sendTextData conn ("Wrong announcement" :: Text)
          | any ($ (coerce . fst) client)
              [T.null, T.any isPunctuation, T.any isSpace] ->
                  WS.sendTextData conn ("Name cannot " <>
                        "contain punctuation or whitespace, and " <>
                        "cannot be empty" :: Text)
          | clientExists client clients' ->
                WS.sendTextData conn ("User already exists" :: Text)
          | otherwise -> flip finally disconnect $ do
                modifyMVar_ state $ \s -> do
                   let s' = addClient client s
                   WS.sendTextData conn $
                       "Welcome! Users: " <>
                       T.intercalate ", " (map (coerce . fst) (clients s))
                   broadcast ((coerce . fst) client <> " joined") s'
                   return s'
                talk client state
         where
           prefix     = "Hi! I am "
           client     = (ClientName (T.drop (T.length prefix) msg), conn)
           disconnect = do
               -- Remove client and return new state
               s <- modifyMVar state $ \s ->
                   let s' = removeClient client s in return (s', s')
               broadcast ((coerce . fst) client <> " disconnected") s

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    modifyMVar_ state $ \s -> do
      let newState = appendEvent (Event msg) s
      print (events newState)
      pure newState
    readMVar state >>= broadcast
        ((coerce user) `mappend` ": " `mappend` msg)
