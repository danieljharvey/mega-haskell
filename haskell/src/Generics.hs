{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics where

import qualified Data.Aeson as JSON
import Data.Kind
import qualified Data.Text as T
import GHC.Generics

data Action
  = NoOp
  | OneArgs {name :: String}
  | TwoArgs {name :: String, age :: Int}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (GenericShow Action)

newtype GenericShow a = GenericShow {getGenericShow :: a}

instance (Generic a, GShow (Rep a)) => Show (GenericShow a) where
  show a = gShow a

class GShow (rep :: Type -> Type) where
  gShow :: rep p -> String

instance GShow (M1 D meta rest) where
  gShow (M1 a) = show a
{-
-- we want these to go to/from json like such
--
-- { type: "NoOp" }
-- { type: "OneArgs", payload: { name: string } }
-- { type: "TwoArgs", payload: { name: string, age: number } }

newtype GenericToJSON a
  = GenericToJSON {getGenericToJSON :: a}

instance (Generic a, GToJSON (Rep a)) => JSON.ToJSON (GenericToJSON a) where
  toJSON (GenericToJSON a) = genericToJSON (from a)

class GToJSON (rep :: Type -> Type) where
  genericToJSON :: rep p -> JSON.Value

instance (JSON.ToJSON a, GToJSON (Rep a), Show a) => GToJSON (K1 R a) where
  genericToJSON (K1 a) = JSON.String (T.pack (show a))

instance (GToJSON rep) => GToJSON (M1 d meta rep) where
  genericToJSON _ = JSON.Null

instance (GToJSON l, GToJSON r) => GToJSON (l :*: r) where
  genericToJSON _ = JSON.Null

instance (GToJSON l, GToJSON r) => GToJSON (l :+: r) where
  genericToJSON _ = JSON.Null

instance (JSON.ToJSON (Rep a p)) => GToJSON a where
  genericToJSON a = JSON.toJSON

poo = genericToJSON (from (GenericToJSON NoOp))

-}

{-
newtype Generically a
  = Generically {loseAllWillToLive :: a}

instance (Generic a, GArbitrary (Rep a)) => Arbitrary (Generically a) where
  arbitrary = Generically . to <$> genericArbitrary

class GArbitrary (rep :: Type -> Type) where
  genericArbitrary :: Gen (rep p)

instance (GArbitrary rep) => GArbitrary (M1 d meta rep) where
  genericArbitrary = M1 <$> genericArbitrary

instance (GArbitrary l, GArbitrary r) => GArbitrary (l :*: r) where
  genericArbitrary = (:*:) <$> genericArbitrary <*> genericArbitrary

instance (GArbitrary l, GArbitrary r) => GArbitrary (l :+: r) where
  genericArbitrary = arbitrary >>= \case
    True -> L1 <$> genericArbitrary
    False -> R1 <$> genericArbitrary

instance (Arbitrary a) => GArbitrary (K1 R a) where
  genericArbitrary = K1 <$> arbitrary-
-}
