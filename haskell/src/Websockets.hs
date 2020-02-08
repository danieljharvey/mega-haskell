{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Websockets where

import Control.Concurrent
  ( MVar,
    forkIO,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
  )
import qualified Control.Concurrent.Async.Timer as Timer
import Control.Exception (finally)
import Control.Monad (forM_, forever, when)
import qualified Data.Aeson as JSON
import Data.Char (isPunctuation, isSpace)
import Data.Coerce
import qualified Data.Map as M
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    listToMaybe,
  )
import Data.Monoid (mappend)
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable
import GHC.Generics
import qualified Network.WebSockets as WS

type Client = (ClientName, WS.Connection)

type EventList = M.Map Int Event

newtype ClientName
  = ClientName {getClientName :: Text}
  deriving (Eq, Ord, Show)

data ServerState
  = ServerState
      { clients :: [Client],
        events :: EventList
      }

newtype Event
  = Event {getEvent :: Text}
  deriving (Eq, Ord, Show)

data Channel a
  = Channel
      { channelName :: Text,
        go :: (ServerState -> Text -> IO ()) -> MVar ServerState -> IO ()
      }

data Tick
  = Tick
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

tickChannel :: Channel Tick
tickChannel =
  Channel
    { channelName = "TickChannel",
      go = \broadcast' stateVar -> do
        let conf = (Timer.setInitDelay 500) . (Timer.setInterval 1000) $ Timer.defaultConf
        Timer.withAsyncTimer conf $ \timer -> do
          forever $ do
            Timer.wait timer
            readMVar stateVar >>= flip broadcast' "Tick"
            pure ()
    }

runChannel ::
  (ServerState -> Text -> IO ()) ->
  MVar ServerState ->
  Channel a ->
  IO ()
runChannel broadcast' stateVar (Channel name go) = do
  print $ "Starting channel " <> name
  go broadcast' stateVar

data BasicUser
  = BasicUser
      { firstName :: Text,
        surname :: Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

type BasicUserProjection = StatefulProjection BasicUser [BasicUser]

-- just adds user to the list
basicUserProjection :: Projection BasicUser [BasicUser]
basicUserProjection =
  Projection
    { reducer = (:),
      def = mempty,
      title = "Basic user projection"
    }

type HorseCountProjection = StatefulProjection BasicUser Int

horseCountProjection :: Projection BasicUser Int
horseCountProjection =
  Projection
    { reducer = \user count ->
        if (firstName user == "Horse" || surname user == "Horse")
          then count + 1
          else count,
      def = 0,
      title = "Horse count projection"
    }

-- basic Projection
-- maybe build in last event so we can use 'def' as state?
data Projection dataType stateType
  = Projection
      { reducer :: (dataType -> stateType -> stateType),
        def :: stateType,
        title :: Text
      }

data StatefulProjection dataType stateType
  = StatefulProjection
      { projection :: Projection dataType stateType,
        value :: MVar (Int, stateType)
      }

createMVar :: Projection dataType stateType -> IO (StatefulProjection dataType stateType)
createMVar projection = do
  mvar <- newMVar (0, (def projection))
  pure $
    StatefulProjection
      { projection = projection,
        value = mvar
      }

class RunProjection a where
  run :: EventList -> a -> IO ()

instance (JSON.FromJSON dataType, Eq stateType, Show stateType, RunProjection rest) => RunProjection (StatefulProjection dataType stateType, rest) where
  run events ((StatefulProjection projection value), rest) = do
    modifyMVar_
      value
      ( \(startKey, oldState) -> do
          let (nextKey, newState) = runProjection events startKey oldState projection
          when (newState /= oldState) $ print newState
          pure (nextKey, newState)
      )
    run events rest

instance RunProjection () where
  run _ _ = pure ()

projections :: IO (BasicUserProjection, (HorseCountProjection, ()))
projections = do
  basicUser <- createMVar basicUserProjection
  horseCount <- createMVar horseCountProjection
  pure (basicUser, (horseCount, ()))

-- run all events
runProjection ::
  (JSON.FromJSON dataType) =>
  EventList ->
  Int ->
  stateType ->
  Projection dataType stateType ->
  (Int, stateType)
runProjection events startKey oldState projection =
  ((nextKey events), newState)
  where
    newState =
      foldr (reducer projection) oldState (usefulEvents events)
    filterOldEvents =
      M.filterWithKey (\k _ -> k >= startKey)
    usefulEvents =
      catMaybes
        . (map (JSON.decode . convertString . getEvent))
        . M.elems
        . filterOldEvents

runProjectionAndPrint ::
  (JSON.FromJSON dataType, Show stateType) =>
  EventList ->
  Projection dataType stateType ->
  IO ()
runProjectionAndPrint events projection = do
  print $ (title projection) <> ":: "
  print $ (runProjection events 1 (def projection)) projection

nextKey :: EventList -> Int
nextKey =
  (+ 1)
    . (fromMaybe 0)
    . listToMaybe
    . reverse
    . M.keys

appendEvent :: Event -> ServerState -> ServerState
appendEvent event serverState =
  serverState {events = M.insert key event (events serverState)}
  where
    key =
      nextKey (events serverState)

newServerState :: ServerState
newServerState = ServerState [] mempty

numClients :: ServerState -> Int
numClients = length . clients

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst) . clients

addClient :: Client -> ServerState -> ServerState
addClient client serverState =
  serverState {clients = client : (clients serverState)}

removeClient :: Client -> ServerState -> ServerState
removeClient client serverState =
  serverState
    { clients = filter ((/= fst client) . fst) (clients serverState)
    }

broadcast :: ServerState -> Text -> IO ()
broadcast serverState message = do
  T.putStrLn message
  forM_ (clients serverState) $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
  state <- newMVar newServerState
  tickThread <- forkIO $ runChannel broadcast state tickChannel
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  msg <- WS.receiveData conn
  clients' <- readMVar state
  case msg of
    _
      | not (prefix `T.isPrefixOf` msg) ->
        WS.sendTextData conn ("Wrong announcement" :: Text)
      | any
          ($ (coerce . fst) client)
          [T.null, T.any isPunctuation, T.any isSpace] ->
        WS.sendTextData
          conn
          ( "Name cannot "
              <> "contain punctuation or whitespace, and "
              <> "cannot be empty" ::
              Text
          )
      | clientExists client clients' ->
        WS.sendTextData conn ("User already exists" :: Text)
      | otherwise -> flip finally disconnect $ do
        modifyMVar_ state $ \s -> do
          let s' = addClient client s
          WS.sendTextData conn $
            "Welcome! Users: "
              <> T.intercalate ", " (map (coerce . fst) (clients s))
          broadcast s' ((coerce . fst) client <> " joined")
          return s'
        talk client state
      where
        prefix = "Hi! I am "
        client = (ClientName (T.drop (T.length prefix) msg), conn)
        disconnect = do
          -- Remove client and return new state
          s <- modifyMVar state $ \s ->
            let s' = removeClient client s in return (s', s')
          broadcast s ((coerce . fst) client <> " disconnected")

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = do
  projections' <- projections
  forever $ do
    msg <- WS.receiveData conn
    modifyMVar_ state $ \s -> do
      let newState = appendEvent (Event msg) s
      -- print (events newState)
      run (events newState) projections'
      pure newState
    readMVar state
      >>= ( \s' ->
              broadcast
                s'
                ((coerce user) `mappend` ": " `mappend` msg)
          )
