{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Viewfunctor where

import           Control.Applicative
import           Control.Applicative.ListF
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.Kind
import           Data.Maybe
import           Data.Monoid               (First (..))
import           Data.Proxy

data JSXTag
  = H1
  | Div
  | Fragment
  deriving (Show)

data JSX
  = Tag JSXTag [JSX]
  | Text String
  deriving (Show)

instance Semigroup JSX where
  a <> b = Tag Fragment [a, b]

instance Monoid JSX where
  mempty = Tag Fragment []

data State
  = State
      { name     :: String
      , age      :: Int
      , loggedIn :: Bool
      }

data LoggedIn a
 deriving (Functor)

data NotLoggedIn a
 deriving (Functor)

type Header
  = Sum LoggedIn NotLoggedIn

type Items
  = ListF ListItem

data ListItem a
  deriving (Functor)

data MainArea a
  deriving (Functor)

type Website
  = Product Header MainArea

class (Functor f) => Render (f :: Type -> Type) (s :: Type) where
  render :: Proxy f -> s -> JSX

class (Functor f) => MaybeRender (f :: Type -> Type) (s :: Type) where
  maybeRender :: Proxy f -> s -> Maybe JSX

instance {-# OVERLAPPABLE #-} (Functor f, Render f s) => MaybeRender f s where
  maybeRender f s = Just $ render (Proxy :: Proxy f) s

instance MaybeRender LoggedIn State where
  maybeRender _ s
    = if loggedIn s
      then Just $ Text "Logged in"
      else Nothing

instance Render NotLoggedIn s where
  render _ _ = Text "Not logged in"

instance Render MainArea State where
  render _ s
    = Tag Div
        [ Tag Div [ Text "Horse" ]
        , Tag Div [ Text "Dogs" ]
        , Tag H1 [ Text $ name s ]
        ]

instance (MaybeRender a s, MaybeRender b s)
  => Render (Sum a b) s where
  render _ s
    = fromMaybe mempty
        $ getFirst $ foldMap First
          [ maybeRender (Proxy :: Proxy a) s
          , maybeRender (Proxy :: Proxy b) s
          ]

instance (Render a s, Render b s) => Render (Product a b) s where
  render _ s
    = render (Proxy :: Proxy a) s <> render (Proxy :: Proxy b) s

poo = render (Proxy :: Proxy Website) State { name = "Mr Horse", age = 100, loggedIn = False }

poo2 = render (Proxy :: Proxy Website) State { name = "Mrs Horse", age = 100, loggedIn = True }
