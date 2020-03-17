module Reader where

import Data.Semigroup
import Prelude

newtype Reader r a
  = Reader {runReader :: r -> a}

reader :: Reader String String
reader = Reader (\r -> "Hello, " <> r)

basic :: String
basic = runReader reader "Dog"

-- "Hello Dog"

instance Functor (Reader r) where
  fmap f (Reader a) = Reader (f <$> a)

functor :: String
functor =
  runReader (fmap (++ "!!!!") reader) "Dog"

instance Applicative (Reader r) where

  pure a = Reader (const a)

  (Reader f) <*> (Reader a) = Reader (\r -> f r (a r))

-- lift the length function into Reader context
-- it will ignore the `r` that is passed in
func :: Reader String (String -> Int)
func = pure length

-- run func on reader, and then pass in "Dog"
applicative :: Int
applicative = runReader (func <*> reader) "Dog"

-- applicative == 10

instance Monad (Reader r) where
  (Reader m) >>= k =
    Reader $ \r -> runReader (k (m r)) r

ask :: Reader a a
ask = Reader id

monad :: Reader String String
monad = do
  name <- ask
  first <- reader
  second <- reader
  pure $ name ++ ": " ++ first ++ "/n" ++ second

runMonad :: String
runMonad = runReader monad "Log"
-- runMonad == "Log: Hello, Log/nHello, Log"
