module FoldEvents where

import Prelude
 
import Data.Array (find) 
import Data.Array.NonEmpty (NonEmptyArray, last, singleton, snoc)
import Data.Either (Either(..)) 
import Data.Foldable (foldMap, foldl) 
import Data.Lens (Lens', over, view) 
 
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust) 
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype, unwrap) 
import Data.Symbol (SProxy(..))

---

newtype PrismControl rawType niceType err
  = PrismControl (PrismControl' rawType niceType err)

type PrismControl' rawType niceType err
  = { events     :: Array Event
    , values     :: NonEmptyArray rawType
    , to         :: niceType -> rawType
    , eitherFrom :: rawType -> Either err niceType
    }

derive instance newtypePrismControl :: Newtype (PrismControl a b c) _

---

_events 
  :: forall rawType a e
   . Lens' (PrismControl rawType a e) (Array Event)
_events 
  =   _Newtype 
  <<< prop (SProxy :: SProxy "events")

---

_values 
  :: forall rawType a e
   . Lens' (PrismControl rawType a e) (NonEmptyArray rawType)
_values 
  =   _Newtype 
  <<< prop (SProxy :: SProxy "values")


---

_to 
  :: forall niceType rawType e
   . Lens' (PrismControl rawType niceType e) (niceType -> rawType)
_to 
  =   _Newtype
  <<< prop (SProxy :: SProxy "to")

---

_eitherFrom 
  :: forall niceType rawType e
   . Lens' (PrismControl rawType niceType e) (rawType -> Either e niceType)
_eitherFrom 
  =   _Newtype
  <<< prop (SProxy :: SProxy "eitherFrom")

--

data Event 
  = OnBlur
  | OnFocus

derive instance eqEvent :: Eq Event
instance showEvent :: Show Event where
  show OnBlur = "OnBlur"
  show OnFocus = "OnFocus"

---

hasFocus 
  :: forall rawType niceType e
   . PrismControl rawType niceType e
  -> Boolean
hasFocus a 
  = foldl hasFocusFold false (view _events a)

hasFocusFold :: Boolean -> Event -> Boolean
hasFocusFold _ value
  = case value of
         OnFocus -> true
         OnBlur  -> false

---

hasBlurred 
  :: forall rawType niceType e
   . PrismControl rawType niceType e
  -> Boolean
hasBlurred 
  = isJust <<< find ((==) OnBlur) <<< view _events

---

hasHadFocus 
  :: forall rawType niceType e 
   . PrismControl rawType niceType e
  -> Boolean
hasHadFocus 
  = isJust <<< find ((==) OnFocus) <<< view _events

---

mostRecentValue 
  :: forall rawType niceType e 
   . PrismControl rawType niceType e
  -> rawType
mostRecentValue = last <<< view _values

---

lastGoodValue
  :: forall rawType niceType e
   . PrismControl rawType niceType e
  -> Maybe niceType
lastGoodValue a
  = unwrap $ foldMap Last convertedValues

  where
    convertedValues
      = map (eitherToMaybe <<< (view _eitherFrom a)) (view _values a)
    
    eitherToMaybe
      = case _ of
          Right a' -> Just a'          
          _        -> Nothing
---

-- | Get the current value and check if it is valid
currentValue
  :: forall rawType niceType e
   . PrismControl rawType niceType e
  -> Either e niceType
currentValue a = view _eitherFrom a (mostRecentValue a)

-- | External API - create a new PrismControl with getter/setters
-- | and a default rawValue
createEmpty 
  :: forall rawType niceType e
   . rawType
  -> (niceType -> rawType)
  -> (rawType -> Either e niceType)
  -> PrismControl rawType niceType e
createEmpty default to from
  = PrismControl
      { events:    mempty
      , values: singleton default
      , eitherFrom: from
      , to:        to
      }

-- | Log a predefined DOM event
-- | Ie, onBlur, onFocus, etc
logEvent 
  :: forall rawType niceType e
   . Event
  -> PrismControl rawType niceType e
  -> PrismControl rawType niceType e
logEvent a 
  = over _events (\as -> (as <> (pure a)))

-- | Log a change in value
logValue
  :: forall rawType niceType e
   . rawType
  -> PrismControl rawType niceType e
  -> PrismControl rawType niceType e
logValue a
  = over _values (flip snoc a)

