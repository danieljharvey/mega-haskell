module Refined where

import Control.Monad ((>=>))
import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Ord ((<), (<=), (>), (>=))
import Data.Show (class Show)
import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Num.Reps (D0)
import Data.Typelevel.Undefined (undefined)
import Data.Int (toNumber)

data RefinedError
  = RefinedError

derive instance eqRefinedError :: Eq RefinedError

instance showRefinedError :: Show RefinedError where
  show RefinedError = "RefinedError"

class Predicate p x where
  validate :: p -> x -> Either RefinedError x

data IdPred

instance predicateIdPred :: Predicate IdPred Number
  where
    validate _ x = Right x

data And l r

instance predicateAnd :: (Predicate l Number, Predicate r Number) => Predicate (And l r)
  Number where
    validate _ x
      = (first >=> second) x
      where
        first x'
          = validate (undefined :: l) x'
        second x'
          = validate (undefined :: r) x'


data Or l r

instance predicateOr :: (Predicate l Number, Predicate r Number) => Predicate (Or l r)
  Number where
    validate _ x
      = first x <|> second x
      where
        first x'
          = validate (undefined :: l) x'
        second x'
          = validate (undefined :: r) x'

data Not a

instance predicateNot :: (Predicate a Number) => Predicate (Not a) Number where
  validate _ x
    = case validate (undefined :: a) x of
        Left _ -> Right x
        _      -> Left RefinedError


data LessThan n

instance predicateLessThan :: (Nat n) => Predicate (LessThan n) Number where
  validate _ x
    = case x < val of
        true  -> Right x
        false -> Left RefinedError
    where
      val :: Number
      val = toNumber (toInt (undefined :: n))

data GreaterThan n

instance predicateGreaterThan :: (Nat n) => Predicate (GreaterThan n) Number
  where
    validate _ x
      = case x > val of
          true -> Right x
          false -> Left RefinedError
      where
        val :: Number
        val = toNumber (toInt (undefined :: n))

data From n

instance predicateFrom :: (Nat n) => Predicate (From n) Number where
  validate _ x
    = case x >= val of
        true  -> Right x
        false -> Left RefinedError
    where
      val :: Number
      val = toNumber (toInt (undefined :: n))

data To n

instance predicateTo :: (Nat n) => Predicate (To n) Number where
  validate _ x
    = case x <= val of
        true  -> Right x
        false -> Left RefinedError
    where
      val :: Number
      val = toNumber (toInt (undefined :: n))



type Positive = GreaterThan D0
