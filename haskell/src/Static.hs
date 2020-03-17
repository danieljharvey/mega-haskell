{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Static where

import Control.Applicative
import Data.Maybe
import GHC.TypeLits

-- API specification DSL
data Get (a :: *)

data a :<|> b = a :<|> b

infixr 8 :<|>

data (a :: k) <//> (b :: *)

infixr 9 <//>

data Capture (a :: *)

-- Example API
type MySite =
  "home" <//> Get Page
    :<|> "blog" <//> Capture BlogId <//> Get Page
    :<|> News

type News =
  "news"
    <//> ( Get Page
             :<|> "more" <//> Get Page
             :<|> "poo" <//> Get Page
         )

-- stupid content type for now
newtype Page
  = Page {getPage :: String}
  deriving (Eq, Ord)

instance Show Page where
  show p = getPage p

newtype BlogId
  = BlogId {getBlogId :: Int}
  deriving newtype (Eq, Ord, Show, Read)

data Proxy a = Proxy

-- The Server type family
type family Server layout :: *

type instance Server (Get a) = IO a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) <//> r) = Server r

type instance Server (Capture a <//> r) = a -> Server r

-- Handler for the example API
handleHome :: IO Page
handleHome = pure (Page "it is the home page")

handleBlog :: BlogId -> IO Page
handleBlog (BlogId id') =
  pure (Page $ "It is blog number " <> show id')

handleNewsPage :: IO Page
handleNewsPage = pure (Page "news")

handleMore :: IO Page
handleMore = pure (Page "more")

handlePoo :: IO Page
handlePoo = pure (Page "poo")

handleMyAPI :: Server MySite
handleMyAPI =
  handleHome
    :<|> handleBlog
    :<|> handleNews

handleNews :: Server News
handleNews =
  ( handleNewsPage
      :<|> handleMore
      :<|> handlePoo
  )

serveMyAPI :: [String] -> IO Page
serveMyAPI = serve (Proxy :: Proxy MySite) handleMyAPI

makeBlog :: IO [Page]
makeBlog = do
  home <- serveMyAPI ["home"]
  page1 <- serveMyAPI ["blog", "1"]
  page2 <- serveMyAPI ["blog", "2"]
  news <- serveMyAPI ["news"]
  more <- serveMyAPI ["news", "more"]
  poo <- serveMyAPI ["news", "poo"]
  pure [home, page1, page2, news, more, poo]

-- The HasServer class
class HasServer layout where
  route ::
    Proxy layout ->
    Server layout ->
    [String] ->
    Maybe (IO Page)

serve ::
  (HasServer layout) =>
  Proxy layout ->
  Server layout ->
  [String] ->
  IO Page
serve p h xs =
  fromMaybe (ioError (userError "404")) (route p h xs)

instance Show a => HasServer (Get a) where
  route ::
    Proxy (Get a) ->
    IO a ->
    [String] ->
    Maybe (IO Page)
  route _ handler [] = Just (Page . show <$> handler)
  route _ _ _ = Nothing

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route ::
    Proxy (a :<|> b) ->
    (Server a :<|> Server b) ->
    [String] ->
    Maybe (IO Page)
  route _ (handlera :<|> handlerb) xs =
    route (Proxy :: Proxy a) handlera xs
      <|> route (Proxy :: Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) <//> r) where
  route ::
    Proxy (s <//> r) ->
    Server r ->
    [String] ->
    Maybe (IO Page)
  route _ handler (x : xs)
    | symbolVal (Proxy :: Proxy s) == x =
      route (Proxy :: Proxy r) handler xs
  route _ _ _ =
    Nothing

instance (Read a, HasServer r) => HasServer (Capture a <//> r) where
  route ::
    Proxy (Capture a <//> r) ->
    (a -> Server r) ->
    [String] ->
    Maybe (IO Page)
  route _ handler (x : xs) = do
    route (Proxy :: Proxy r) (handler (read x)) xs
  route _ _ _ = Nothing
