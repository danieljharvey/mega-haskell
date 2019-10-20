{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Typelevel where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Vector as V

newtype NiceString
  = NiceString {getNice :: String}

instance Show NiceString where
  show (NiceString s) = s

-- here is a type level natural number
data Nat = Zero | Succ Nat

-- and a function for adding them together
-- each time we try and move stuff from left to right pile
-- so everything still adds up, as such
type family (m :: Nat) + (n :: Nat) where
  'Zero + x = x
  'Succ a + b = a + 'Succ b

data Vector (m :: Nat) (a :: *) where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector m a -> Vector ('Succ m) a

instance Show a => Show (Vector n a) where
  show VNil = "VNil"
  show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

instance Functor (Vector n) where
  fmap _ VNil = VNil
  fmap f (VCons a as) = VCons (f a) (fmap f as)

vectyBoy :: Vector (Succ (Succ (Succ (Succ Zero)))) Int
vectyBoy = VCons 1 (VCons 2 (VCons 3 (VCons 4 VNil)))

emptyBoy :: Vector Zero Int
emptyBoy = VNil

-- look at this unreasonable type signature
longBoy :: Vector (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))) Int
longBoy = vAppend vectyBoy vectyBoy

mappedBoy :: Vector (Succ (Succ (Succ (Succ Zero)))) Int
mappedBoy = (* 10) <$> vectyBoy

-- VCons 10 (VCons 20 (VCons 30 (VCons 40 (VNil))))

-- like the nats, we are moving 'a' from the left pile into the right pile
-- when everything is in the right pile, return it
-- note we don't ever bother thinking about the Nats
-- as VCons takes care of incrementing them
vAppend :: Vector n a -> Vector m a -> Vector (n + m) a
vAppend VNil x = x
vAppend (VCons a rest) as = vAppend rest (VCons a as)

-- we can guarantee a value from the head function
-- because it specifically uses Succ in the type signature
vHead :: Vector (Succ n) a -> a
vHead (VCons a _) = a

a = vHead vectyBoy

-- a == 1

-- b = vHead emptyBoy
-- Couldn't match type ‘'Zero’ with ‘'Succ n0’
--      Expected type: Vector ('Succ n0) Int
--        Actual type: Vector 'Zero Inti

type family (m :: Nat) - (n :: Nat) :: Nat where
  m - Zero = m
  Succ m - Succ n = m - n
