module Refined where

import Prelude

import Control.Alternative ((<|>))
import Data.Either (Either(..))

import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Num.Reps (D0, D1)
import Data.Typelevel.Undefined (undefined)
import Data.Int (toNumber)
import Data.Foldable (class Foldable, length)
import Data.Bifunctor (lmap)

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Generic.Rep (class Generic)

data RefinedError
  = RefinedError

derive instance eqRefinedError 
  :: Eq RefinedError

instance showRefinedError :: Show RefinedError where
  show RefinedError = "RefinedError"

-- there must be a better way to number constrain here
class Comparable a where
  fromInt :: Int -> a

instance comparableInt :: Comparable Int where
  fromInt x = x 

instance comparableNumber :: Comparable Number where
  fromInt = toNumber

newtype Refined p x
  = Refined x

derive newtype instance eqRefined 
  :: (Eq x) => Eq (Refined p x)
derive newtype instance showRefined 
  :: (Show x) => Show (Refined p x)
derive instance genericRefined 
  :: Generic (Refined p x) _

-- for decoding we first decode the thing inside, then run our predicate on it
instance decodeJsonRefined 
  :: (DecodeJson x, Predicate p x) 
  => DecodeJson (Refined p x) where
  decodeJson a = do
     val <- decodeJson a
     (refineStr val :: Either String (Refined p x))

-- for encoding we just want to strip away the outside layer and use whatever
-- is inside
derive newtype instance encodeJsonRefined 
  :: (EncodeJson x) => EncodeJson (Refined p x)

refineStr 
  :: forall p x. (Predicate p x) 
   => x 
   -> Either String (Refined p x)
refineStr x = lmap show (refine x)

refine 
  :: forall p x. (Predicate p x) 
  => x 
  -> Either RefinedError (Refined p x)
refine x = do
  Refined <$> validate (undefined :: p) x

unsafeRefine 
  :: forall p x. (Predicate p x) 
   => x 
   -> (Refined p x)
unsafeRefine x = Refined x

unrefine 
  :: forall p x. Refined p x 
   -> x
unrefine (Refined x) = x

class Predicate p x where
  validate :: p -> x -> Either RefinedError x

---

data IdPred

instance predicateIdPred 
  :: Predicate IdPred x
  where
    validate _ x = Right x

---

data And l r

instance predicateAnd 
  :: (Predicate l x, Predicate r x) 
  => Predicate (And l r) x where
    validate _ x
      = (first >=> second) x
      where
        first x'
          = validate (undefined :: l) x'
        second x'
          = validate (undefined :: r) x'

---

data Or l r

instance predicateOr 
  :: (Predicate l x, Predicate r x) 
  => Predicate (Or l r) x where
    validate _ x
      = first x <|> second x
      where
        first x'
          = validate (undefined :: l) x'
        second x'
          = validate (undefined :: r) x'

---

data SizeEqualTo a

instance predicateSizeEqualTo 
  :: (Predicate a x, Foldable t, Nat n) 
  => Predicate (SizeEqualTo n) (t a) where
    validate _ x
      = case length x == val of
          true  -> Right x
          false -> Left RefinedError
      where
        val :: Int
        val = toInt (undefined :: n)

---

data SizeGreaterThan a

instance predicateSizeGreaterThan 
  :: (Predicate a x, Foldable t, Nat n) 
  => Predicate (SizeGreaterThan n) (t a) where
    validate _ x
      = case length x > val of
          true  -> Right x
          false -> Left RefinedError
      where
        val :: Int
        val = toInt (undefined :: n)

---

data SizeLessThan a

instance predicateSizeLessThan 
  :: (Predicate a x, Foldable t, Nat n) 
  => Predicate (SizeLessThan n) (t a) where
    validate _ x
      = case length x < val of
          true  -> Right x
          false -> Left RefinedError
      where
        val :: Int
        val = toInt (undefined :: n)

---

data Not a

instance predicateNot 
  :: (Predicate a x) 
  => Predicate (Not a) x where
  validate _ x
    = case validate (undefined :: a) x of
        Left _ -> Right x
        _      -> Left RefinedError

---

data LessThan n

instance predicateLessThan 
  :: (Nat n, Ord x, Comparable x) 
  => Predicate (LessThan n) x where
  validate _ x
    = case x < (fromInt val) of
        true  -> Right x
        false -> Left RefinedError
    where
      val 
        = toInt (undefined :: n)

---

data GreaterThan n

instance predicateGreaterThan 
  :: (Nat n, Ord x, Comparable x) 
  => Predicate (GreaterThan n) x
  where
    validate _ x
      = case x > (fromInt val) of
          true -> Right x
          false -> Left RefinedError
      where
        val 
          = toInt (undefined :: n)

---

data From n

instance predicateFrom 
  :: (Nat n, Ord x, Comparable x) 
  => Predicate (From n) x where
  validate _ x
    = case x >= (fromInt val) of
        true  -> Right x
        false -> Left RefinedError
    where
      val 
        = toInt (undefined :: n)

---

data To n

instance predicateTo 
  :: (Nat n, Ord x, Comparable x) 
  => Predicate (To n) x where
  validate _ x
    = case x <= (fromInt val) of
        true  -> Right x
        false -> Left RefinedError
    where
      val 
        = toInt (undefined :: n)

---

data FromTo m n

instance predicateFromTo 
  :: (Nat n, Nat m, Ord x, Comparable x) 
  => Predicate (FromTo m n) x where
  validate _ x
    = case (x >= fromInt lower) && (x <= fromInt upper) of
        true  -> Right x
        false -> Left RefinedError
    where
      lower 
        = toInt (undefined :: m)
      upper
        = toInt (undefined :: n)

---

data EqualTo n

instance predicateEqualTo 
  :: (Nat n, Eq x, Comparable x) 
  => Predicate (EqualTo n) x where
  validate _ x
    = case x == (fromInt val) of
        true -> Right x
        false -> Left RefinedError
    where
      val 
        = toInt (undefined :: n)

---

data NotEqualTo n

instance predicateNotEqualTo 
  :: (Nat n, Eq x, Comparable x) 
  => Predicate (NotEqualTo n) x where
  validate _ x
    = case x /= (fromInt val) of
        true -> Right x
        false -> Left RefinedError
    where
      val 
        = toInt (undefined :: n)

-- | A 'Predicate' ensuring that the value is greater than zero.
type Positive = GreaterThan D0

-- | A 'Predicate' ensuring that the value is less than or equal to zero.
type NonPositive = To D0

-- | A 'Predicate' ensuring that the value is less than zero.
type Negative = LessThan D0

-- | A 'Predicate' ensuring that the value is greater than or equal to zero.
type NonNegative = From D0

-- | An inclusive range of values from zero to one.
type ZeroToOne = FromTo D0 D1

-- | A 'Predicate' ensuring that the value is not equal to zero.
type NonZero = NotEqualTo D0

-- | A 'Predicate' ensuring that the 'Foldable' is non-empty.
type NonEmpty = SizeGreaterThan D0
