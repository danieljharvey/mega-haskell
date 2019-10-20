module Alternative where

import Control.Applicative
import Data.Char (toLower)
import Data.List (isInfixOf)

data Perhaps a = Yeah a | Nope deriving (Eq, Show)

instance Functor Perhaps where
  fmap f (Yeah a) = Yeah (f a)
  fmap _ _ = Nope

instance Applicative Perhaps where

  pure = Yeah

  (Yeah f) <*> (Yeah a) = Yeah (f a)
  _ <*> _ = Nope

instance Alternative Perhaps where

  empty = Nope

  Yeah x <|> _ = Yeah x
  _ <|> y = y

-- test functions

first :: [a] -> Perhaps a
first (a : _) = Yeah a
first _ = Nope

second :: [a] -> Perhaps a
second (_ : b : _) = Yeah b
second _ = Nope

naiveImplementation :: [a] -> Perhaps a
naiveImplementation as = case second as of
  Yeah s -> Yeah s
  Nope -> first as

-- combining them with <|>

getPreferred :: [a] -> Perhaps a
getPreferred as = second as <|> first as

-- answers

nah :: Perhaps Int
nah = getPreferred []

-- nah == Nope

found :: Perhaps Int
found = getPreferred [1, 2]

-- found = Yeah 2

fallback :: Perhaps Int
fallback = getPreferred [1]

-- fallback == Yeah 1

type Url = String

type Match = String

data Route
  = Index
  | Gallery
  | Contact
  | Complaints
  | Help
  deriving (Eq, Show)

matches :: Match -> Route -> Url -> Perhaps Route
matches match route url =
  if isInfixOf match' url'
    then Yeah route
    else Nope
  where
    match' = toLower <$> match
    url' = toLower <$> url

matchRouteDefault :: Url -> Route
matchRouteDefault url =
  case matchRoute url of
    Yeah route -> route
    _ -> Index

matchRoute :: Url -> Perhaps Route
matchRoute url =
  matches "gallery" Gallery url
    <|> matches "contact" Contact url
    <|> matches "complaints" Complaints url
    <|> matches "help" Help url

findGallery :: Route
findGallery = matchRouteDefault "http://internet.com/gallery"

-- findGallery == Gallery

findDefault :: Route
findDefault = matchRouteDefault "http://internet.com/rubbish"
-- findDefault == Index
