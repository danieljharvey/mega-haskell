module Data.Monoid.LastE where

import Prelude
import Data.Either (Either(..))
import Data.Newtype

newtype LastE e a
  = LastE (Either e a)

derive newtype instance eqLastE ::
  (Eq a, Eq e) =>
  Eq (LastE e a)

derive newtype instance ordLastE ::
  (Ord a, Ord e) =>
  Ord (LastE e a)

derive newtype instance showLastE ::
  (Show a, Show e) =>
  Show (LastE e a)

derive newtype instance functorLastE ::
  Functor (LastE e)

derive newtype instance applyLastE ::
  Apply (LastE e)

derive newtype instance applicativeLastE ::
  Applicative (LastE e)

derive newtype instance bindLastE ::
  Bind (LastE e)

derive newtype instance monadLastE ::
  Monad (LastE e)

derive instance newtypeLastE :: Newtype (LastE e a) _

instance semigroupLastE ::
  Semigroup (LastE e a) where
  append a (LastE (Left _)) = a
  append _ b = b

instance monoidLastE ::
  (Monoid e) =>
  Monoid (LastE e a) where
  mempty = LastE (Left mempty)
