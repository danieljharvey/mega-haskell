{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Typelevel where

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Vector      as V

newtype NiceString
  = NiceString { getNice :: String }

instance Show NiceString where
  show (NiceString s) = s

-- here is a type level natural number
data Nat = Zero | Succ Nat

-- and a function for adding them together
-- each time we try and move stuff from left to right pile
-- so everything still adds up, as such
type family (m :: Nat) + (n :: Nat) where
  'Zero + x   = x
  'Succ a + b = a + 'Succ b

data Vector (m :: Nat) (a :: *) where
  VNil  :: Vector 'Zero a
  VCons :: a -> Vector m a -> Vector ('Succ m) a

instance Show a => Show (Vector n a) where
  show VNil         = "VNil"
  show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"
{-
instance JSON.ToJSON a => JSON.ToJSON (Vector n a) where
  toJSON VNil         = JSON.emptyArray
  toJSON (VCons a as)
    = mappend (JSON.Array $ V.singleton val) (JSON.toJSON as)
    where
      val :: JSON.Value
      val = JSON.toJSON a
-}

-- like the nats, we are moving 'a' from the left pile into the right pile
-- when everything is in the right pile, return it
-- note we don't ever bother thinking about the Nats
-- as VCons takes care of incrementing them
append :: Vector n a -> Vector m a -> Vector (n + m) a
append VNil x            = x
append (VCons a rest) as = append rest (VCons a as)

