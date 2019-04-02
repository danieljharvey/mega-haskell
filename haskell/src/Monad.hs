module Monad where

import           Control.Monad.Reader
import           Control.Monad.Writer hiding ((<>))
import           Data.List
import           Data.Semigroup       ((<>))

-- simple definition

newtype Id a
  = Id { getId :: a}
  deriving (Eq, Ord, Show)

val :: Id Int
val = Id 7

plainVal :: Int
plainVal = getId val
-- plainVal = 7

-- runs the f function to the a inside Id
instance Functor Id where
  fmap f (Id a) = Id (f a)

doubled :: Id Int
doubled = fmap (*2) val
-- doubled == Id 14

-- pure puts anything inside Id
-- <*> (or "ap") runs a function inside one Id to a value inside another one
instance Applicative Id where
  pure = Id
  (Id f) <*> (Id a) = Id (f a)

idValue :: Id String
idValue = pure "Hello!"
-- idValue = Id "Hello!"

getLength :: Id Int
getLength = Id length <*> Id "Dogs"
-- getLength == Id 4

-- >>= (or bind) runs a function that adds another layer of Id, then flattens it to one layer
-- sometimes called flatMap, because it's maps and then flattens
instance Monad Id where
  (Id m) >>= k  = k m

doubleAndWrap :: Int -> Id Int
doubleAndWrap i
  = pure (i * 2)
-- doubleAndWrap 1 = Id 2

doubleAFewTimes :: Int -> Id Int
doubleAFewTimes i = do
  j <- doubleAndWrap i
  k <- doubleAndWrap j
  l <- doubleAndWrap k
  doubleAndWrap l
-- doubleAFewTimes 10 = 160

-- Maybe example

firstItem :: [a] -> Maybe a
firstItem []    = Nothing
firstItem (a:_) = Just a

head3 :: [[[a]]] -> Maybe a
head3 aaas = do
  aas <- firstItem aaas
  as  <- firstItem aas
  a   <- firstItem as
  pure a
-- head3 []          -- Nothing
-- head3 [[[1,2,3]]] -- Just 1

-- Either example

data Error
  = TooLong
  | ContainsHorse
  | IsEmpty
  deriving (Show, Eq, Ord)

isEmpty :: String -> Either Error String
isEmpty s
  = if null s
    then Left IsEmpty
    else Right s

tooLong :: String -> Either Error String
tooLong s
  = if length s > 10
    then Left TooLong
    else Right s

containsHorse :: String -> Either Error String
containsHorse s
    = if "horse" `isInfixOf` s
    then Left ContainsHorse
    else Right s

validate :: String -> Either Error String
validate s = do
  t <- isEmpty s
  u <- tooLong t
  containsHorse u
-- validate ""                   == Left IsEmpty
-- validate "bah horse"          == Left ContainsHorse
-- validate "really long string" == Left TooLong
-- validate "Hello"              == Right "Hello"

-- List example

moreList :: Int -> [Int]
moreList a = [a - 1, a, a + 1]
-- moreList 1 == [0, 1, 2]

lotsMoreList :: Int -> [Int]
lotsMoreList a = do
  b <- moreList a
  moreList b
-- lotsMoreList 1 == [-1,0,1,0,1,2,1,2,3]

-- Reader example

data Config
  = Config { ipAddress :: String
           , name      :: String
           }

printName :: Reader Config String
printName = do
  config <- ask
  pure ("the name is " <> name config)

printIp :: Reader Config String
printIp = do
  config <- ask
  pure ("The ip address is " <> ipAddress config)

configReader :: Reader Config String
configReader = do
  ip <- printIp
  name <- printName
  pure (ip <> ", " <> name)

config :: Config
config = Config { ipAddress = "127.0.0.1", name = "localhost" }

withConfig :: String
withConfig = runReader configReader config
-- withConfig == "The ip address is 127.0.0.1, the name is localhost"

-- Writer example

addOne :: Int -> Writer String Int
addOne i = do
  tell "Add one "
  pure (i + 1)

timesTwo :: Int -> Writer String Int
timesTwo i = do
  tell "times two "
  pure (i * 2)

maths :: Int -> Writer String Int
maths i = do
  j <- addOne i
  k <- timesTwo j
  pure k
-- runWriter maths 10 == (22, "Add one times two")

-- IO example

things :: IO String
things = do
  _ <- putStrLn "Hello! What is your name?"
  name <- readLn
  putStrLn ("Hello, " ++ name)
  pure name
