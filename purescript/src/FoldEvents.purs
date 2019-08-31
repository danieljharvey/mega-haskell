module FoldEvents where

import Prelude
import Data.Array (find, length)
import Data.Array.NonEmpty (NonEmptyArray, toArray, last, singleton, snoc)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, foldr)
import Data.Lens (Lens', over, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))

---
newtype MiniStore rawType niceType err
  = MiniStore (MiniStore' rawType niceType err)

type MiniStore' rawType niceType err
  = { events :: Array Event
    , values :: NonEmptyArray rawType
    , to :: niceType -> rawType
    , eitherFrom :: rawType -> Either err niceType
    }

derive instance newtypeMiniStore :: Newtype (MiniStore a b c) _

---
_events ::
  forall rawType a e.
  Lens' (MiniStore rawType a e) (Array Event)
_events =
  _Newtype
    <<< prop (SProxy :: SProxy "events")

---
_values ::
  forall rawType a e.
  Lens' (MiniStore rawType a e) (NonEmptyArray rawType)
_values =
  _Newtype
    <<< prop (SProxy :: SProxy "values")

---
_to ::
  forall niceType rawType e.
  Lens' (MiniStore rawType niceType e) (niceType -> rawType)
_to =
  _Newtype
    <<< prop (SProxy :: SProxy "to")

---
_eitherFrom ::
  forall niceType rawType e.
  Lens' (MiniStore rawType niceType e) (rawType -> Either e niceType)
_eitherFrom =
  _Newtype
    <<< prop (SProxy :: SProxy "eitherFrom")

--
data Event
  = OnBlur
  | OnFocus
  | OnSubmit
  | OnClick

derive instance eqEvent :: Eq Event

instance showEvent :: Show Event where
  show OnBlur = "OnBlur"
  show OnFocus = "OnFocus"
  show OnSubmit = "OnSubmit"
  show OnClick = "OnClick"

---
hasFocus ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Boolean
hasFocus a = foldl hasFocusFold false (view _events a)

hasFocusFold :: Boolean -> Event -> Boolean
hasFocusFold current value = case value of
  OnFocus -> true
  OnBlur -> false
  _ -> current

---
hasBlurred ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Boolean
hasBlurred = hasEvent OnBlur

---
hasHadFocus ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Boolean
hasHadFocus = hasEvent OnFocus

---
hasSucceeded ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Boolean
hasSucceeded =
  (_ > 0)
    <<< length
    <<< allGoodValues

---
hasSubmitted ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Boolean
hasSubmitted = hasEvent OnSubmit

hasBeenClicked ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Boolean
hasBeenClicked = hasEvent OnClick

hasEvent ::
  forall rawType niceType e.
  Event ->
  MiniStore rawType niceType e ->
  Boolean
hasEvent event =
  isJust
    <<< find ((==) event)
    <<< view _events

---
mostRecentValue ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  rawType
mostRecentValue =
  last
    <<< view _values

---
lastGoodValue ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Maybe niceType
lastGoodValue a = unwrap $ foldMap Last convertedValues
  where
  convertedValues =
    map
      ( eitherToMaybe
          <<< (view _eitherFrom a)
      )
      (view _values a)

  eitherToMaybe = case _ of
    Right a' -> Just a'
    _ -> Nothing

---
allValues ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  NonEmptyArray rawType
allValues = view _values

---
allGoodValues ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Array niceType
allGoodValues a =
  ( filterRight
      <<< toArray
      <<< map (view _eitherFrom a)
      <<< allValues
  )
    a

---
allGoodValuesRaw ::
  forall rawType niceType e.
  MiniStore rawType niceType e -> Array rawType
allGoodValuesRaw a = map (view _to a) (allGoodValues a)

---
filterRight :: forall e a. Array (Either e a) -> Array a
filterRight = foldr foldFunc mempty
  where
  foldFunc item total = case item of
    Right a -> total <> pure a
    _ -> total

---
-- | Get the current value and check if it is valid
currentValue ::
  forall rawType niceType e.
  MiniStore rawType niceType e ->
  Either e niceType
currentValue a = view _eitherFrom a (mostRecentValue a)

-- | External API - create a new MiniStore with getter/setters
-- | and a default rawValue
createEmpty ::
  forall rawType niceType e.
  rawType ->
  (niceType -> rawType) ->
  (rawType -> Either e niceType) ->
  MiniStore rawType niceType e
createEmpty default to from =
  MiniStore
    { events: mempty
    , values: singleton default
    , eitherFrom: from
    , to: to
    }

-- | Log a predefined DOM event
-- | Ie, onBlur, onFocus, etc
logEvent ::
  forall rawType niceType e.
  Event ->
  MiniStore rawType niceType e ->
  MiniStore rawType niceType e
logEvent a = over _events (\as -> (as <> (pure a)))

-- | Log a change in value
logValue ::
  forall rawType niceType e.
  rawType ->
  MiniStore rawType niceType e ->
  MiniStore rawType niceType e
logValue a = over _values (flip snoc a)
