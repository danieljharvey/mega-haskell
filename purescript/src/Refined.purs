module Refined where

import Prelude

import Control.Alternative ((<|>))
import Data.Either (Either(..))

import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Num.Reps (D0)
import Data.Typelevel.Undefined (undefined)
import Data.Int (toNumber)

import Data.Bifunctor (lmap)

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Generic.Rep (class Generic)

data RefinedError
  = RefinedError

derive instance eqRefinedError :: Eq RefinedError

instance showRefinedError :: Show RefinedError where
  show RefinedError = "RefinedError"

-- there must be a better way to number constrain here
class Numberable a where
  toNum :: a -> Number

instance numberableInt :: Numberable Int where
  toNum = toNumber

instance numberableNumber :: Numberable Number where
  toNum x = x

newtype Refined p x
  = Refined x

derive newtype instance eqRefined :: (Eq x) => Eq (Refined p x)
derive newtype instance showRefined :: (Show x) => Show (Refined p x)
derive instance genericRefined :: Generic (Refined p x) _

-- for decoding we first decode the thing inside, then run our predicate on it
instance decodeJsonRefined :: (DecodeJson x, Predicate p x) => DecodeJson (Refined p x) where
  decodeJson a = do
     val <- decodeJson a
     (refineStr val :: Either String (Refined p x))

-- for encoding we just want to strip away the outside layer and use whatever
-- is inside
derive newtype instance encodeJsonRefined :: (EncodeJson x) => EncodeJson (Refined p x)

refineStr :: forall p x. (Predicate p x) => x -> Either String (Refined p x)
refineStr x = lmap show (refine x)

refine :: forall p x. (Predicate p x) => x -> Either RefinedError (Refined p x)
refine x = do
  Refined <$> validate (undefined :: p) x

unsafeRefine :: forall p x. (Predicate p x) => x -> (Refined p x)
unsafeRefine x = Refined x

unrefine :: forall p x. Refined p x -> x
unrefine (Refined x) = x

class Predicate p x where
  validate :: p -> x -> Either RefinedError x

data IdPred

instance predicateIdPred :: Predicate IdPred x
  where
    validate _ x = Right x

data And l r

instance predicateAnd :: (Predicate l x, Predicate r x) => Predicate (And l r)
  x where
    validate _ x
      = (first >=> second) x
      where
        first x'
          = validate (undefined :: l) x'
        second x'
          = validate (undefined :: r) x'


data Or l r

instance predicateOr :: (Predicate l x, Predicate r x) => Predicate (Or l r)
  x where
    validate _ x
      = first x <|> second x
      where
        first x'
          = validate (undefined :: l) x'
        second x'
          = validate (undefined :: r) x'

data Not a

instance predicateNot :: (Predicate a x) => Predicate (Not a) x where
  validate _ x
    = case validate (undefined :: a) x of
        Left _ -> Right x
        _      -> Left RefinedError


data LessThan n

instance predicateLessThan :: (Nat n, Numberable x) => Predicate (LessThan n) x where
  validate _ x
    = case (toNum x) < val of
        true  -> Right x
        false -> Left RefinedError
    where
      val :: Number
      val = toNumber (toInt (undefined :: n))

data GreaterThan n

instance predicateGreaterThan :: (Nat n, Numberable x) => Predicate (GreaterThan n) x
  where
    validate _ x
      = case (toNum x) > val of
          true -> Right x
          false -> Left RefinedError
      where
        val :: Number
        val = toNumber (toInt (undefined :: n))

data From n

instance predicateFrom :: (Nat n, Numberable x) => Predicate (From n) x where
  validate _ x
    = case (toNum x) >= val of
        true  -> Right x
        false -> Left RefinedError
    where
      val :: Number
      val = toNumber (toInt (undefined :: n))

data To n

instance predicateTo :: (Nat n, Numberable x) => Predicate (To n) x where
  validate _ x
    = case (toNum x) <= val of
        true  -> Right x
        false -> Left RefinedError
    where
      val :: Number
      val = toNumber (toInt (undefined :: n))

data EqualTo n

instance predicateEqualTo :: (Nat n, Numberable x) => Predicate (EqualTo n) x where
  validate _ x
    = case (toNum x) == val of
        true -> Right x
        false -> Left RefinedError
    where
      val :: Number
      val = toNumber (toInt (undefined :: n))

type Positive = GreaterThan D0
