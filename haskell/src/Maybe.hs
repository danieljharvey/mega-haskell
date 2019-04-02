module Maybe where

import           Control.Applicative
import           Control.Monad             hiding (fail)
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Zip
import           Data.Semigroup
import           Prelude                   hiding (Maybe (..), fail)

data Maybe a = Just a | Nothing
  deriving (Eq, Ord, Show)

-- If we have a value, run the function on it
-- if we don't - do nothing
instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing

-- pure = default for datatype.
-- we assume success so Just
-- <*> is for combining functions that are also in Maybe
instance Applicative Maybe where
  pure a = Just a
  (Just f) <*> (Just a) = Just (f a)
  _        <*> _        = Nothing

-- if we get two wrapped maybes, make it into one
-- if we start with Nothing, do nothing else
instance Monad Maybe where
  (Just a) >>= k   = k a
  Nothing  >>= _   = Nothing

-- concatenation of things - the things must themselves be combinable
instance (Semigroup a) => Semigroup (Maybe a) where
  (Just a) <> (Just b) = Just (a <> b)
  Nothing  <> a        = a
  a        <> Nothing  = a

-- empty value is nothing as appending nothing to anything does not change it
instance (Semigroup a) => Monoid (Maybe a) where
  mappend = (<>)
  mempty = Nothing

-- think of this as combining a list that may have one or zero items
instance Foldable Maybe where
  foldr _ a Nothing  = a
  foldr f a (Just b) = f b a

instance Alternative Maybe where
  empty                 = Nothing
  (Just a) <|> _        = Just a
  Nothing  <|> (Just b) = Just b
  Nothing  <|> Nothing  = Nothing

instance Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just a) = fmap Just (f a)

-- provides a general purpose way of failing a computation
instance MonadFail Maybe where
  fail _ = Nothing

-- it's just Alternative again!
instance MonadPlus Maybe where
  mzero = Nothing

  mplus (Just a) _         = Just a
  mplus (Nothing) (Just b) = Just b
  mplus _ _                = Nothing

-- a monad generalising zipLists (combining two sets of things into one)
instance MonadZip Maybe where
  mzipWith = liftA2

-- Our MaybeT monad transformer
newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

-- functor instance
instance (Functor m) => Functor (MaybeT m) where
  fmap f a = MaybeT mapped
    where
      mapped
        = (fmap . fmap) f value

      value
        = runMaybeT a

-- applicative instance
instance (Monad m) => Applicative (MaybeT m) where
  pure a = MaybeT (pure (Just a))

  maybeTF <*> maybeTA = MaybeT $ do
    maybeF <- runMaybeT maybeTF
    case maybeF of
     Nothing -> pure Nothing
     Just f  -> do
       maybeA <- runMaybeT maybeTA
       case maybeA of
         Nothing -> pure Nothing
         Just a  -> pure (Just (f a))

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  x >>= f = MaybeT $ do
    value <- runMaybeT x
    case value of
      Nothing -> pure Nothing
      Just a  -> runMaybeT (f a)

instance (Foldable m) => Foldable (MaybeT m) where
  foldMap f (MaybeT a) = foldMap (foldMap f) a

instance (Traversable m) => Traversable (MaybeT m) where
  traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a

instance (Functor m, Monad m) => Alternative (MaybeT m) where
  empty = MaybeT $ pure Nothing

  a <|> b = MaybeT $ do
    a' <- runMaybeT a
    case a' of
      Nothing -> runMaybeT b
      Just _  -> pure a'

-- allow a computation to be failed
instance (Monad m) => MonadFail (MaybeT m) where
  fail _ = MaybeT $ pure Nothing

-- if M does IO, then this can do IO too
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- get line from terminal, check it's 1 char or more
getNiceLine :: MaybeT IO String
getNiceLine = do
  line <- liftIO readLn
  if (length line) > 0
    then pure line
    else fail "String too short"
