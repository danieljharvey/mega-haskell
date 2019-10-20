module Either where

import Control.Applicative
import Control.Monad.Zip
import Data.Bifunctor
import Data.Semigroup
import Prelude hiding (Either (..))

data Either a b
  = Left a
  | Right b
  deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap f (Right b) = Right (f b)
  fmap _ (Left a) = Left a

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

-- pure = default for datatype.
-- we assume success so Right
-- <*> is for combining functons that are also in Either
instance Applicative (Either a) where

  pure a = Right a

  (Right f) <*> (Right a) = Right (f a)
  (Right f) <*> (Left a) = Left a
  (Left a) <*> _ = Left a

-- if we get two wrapped eithers, make it into one
-- if we start with Left, do nothing else
instance Monad (Either a) where
  Right a >>= k = k a
  Left e >>= _ = Left e

-- concatenation of things - the things must themselves be combinable
instance (Semigroup a) => Semigroup (Either e a) where
  (Right a) <> (Right b) = Right (a <> b)
  (Left a) <> (Left b) = Left a
  (Left a) <> b = b
  a <> (Left b) = a

{- BORKED
instance (Semigroup a, Monoid e) => Monoid (Either e a) where
  mempty = Right mempty -- doesn't work - wouldn't combine with a left properly
-}

-- think of this as combining a list that may have one or zero items
instance Foldable (Either e) where
  foldr _ a (Left e) = a
  foldr f a (Right b) = f b a

{- BORKED
instance (Monoid e) => Alternative (Either e) where
  empty                  = Left mempty
  (Right a) <|> _        = Right a
  Left _   <|> (Right b) = Right b
  Nothing  <|> Nothing   = Nothing
-}

instance Traversable (Either e) where
  traverse _ (Left e) = pure (Left e)
  traverse f (Right a) = fmap Right (f a)

-- provides a general purpose way of failing a computation
{-
 can't have MonadFail (Either e) because e might not be String
instance MonadFail (Either String) where
  fail e = Left e

-- MonadPlus is Alternative and thus doesn't work

-}
-- a monad generalising zipLists (combining two sets of things into one)
instance MonadZip (Either e) where
  mzipWith = liftA2
