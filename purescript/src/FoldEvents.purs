module FoldEvents where

import Prelude
 
import Data.Array (find) 
import Data.Either (Either(..)) 
import Data.Foldable (foldMap, foldl) 
import Data.Lens (Getter', Lens', left, over, view) 
import Data.Lens.Getter (to) 
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
  = { events     :: ControlEvents rawType
    , to         :: niceType -> rawType
    , eitherFrom :: rawType -> Either err niceType
    }

derive instance newtypePrismControl :: Newtype (PrismControl a b c) _

---

type ControlEvents rawType
  = Array (Event rawType)

---

_events 
  :: forall rawType a e
   . Lens' (PrismControl rawType a e) (ControlEvents rawType)
_events 
  =   _Newtype 
  <<< prop (SProxy :: SProxy "events")

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

data Event rawType
  = OnBlur
  | OnFocus
  | OnChange rawType

derive instance eqEvent :: (Eq internalType) => Eq (Event internalType)
instance showEvent :: (Show a) => Show (Event a) where
  show OnBlur = "OnBlur"
  show OnFocus = "OnFocus"
  show (OnChange a) = "OnChange - " <> show a

data EventError e
  = NoValue
  | ValidationError e

derive instance eqEventError :: (Eq a) => Eq (EventError a)

instance showEventError :: (Show a) => Show (EventError a) where
  show NoValue = "NoValue"
  show (ValidationError a) = show a

---

_hasFocus 
  :: forall rawType niceType e
   . Getter' (PrismControl rawType niceType e) Boolean
_hasFocus = _events <<< to (hasFocus)
  where
    hasFocus 
      = foldl reducey false 
      where
        reducey
          = (\t e -> case e of
              OnFocus -> true
              OnBlur  -> false
              _       -> t) 

---

_hasBlurred 
  :: forall rawType niceType e
   . Eq rawType
  => Getter' (PrismControl rawType niceType e) Boolean
_hasBlurred 
  =   _events 
  <<< to (isJust <<< find ((==) OnBlur))

---

_hasHadFocus 
  :: forall rawType niceType e 
   . Eq rawType
  => Getter' (PrismControl rawType niceType e) Boolean
_hasHadFocus 
  =   _events 
  <<< to (isJust <<< find ((==) OnFocus))

---

mostRecentValue 
  :: forall i e
   . ControlEvents i 
  -> Either (EventError e) i
mostRecentValue events
  = case unwrap $ foldMap Last (map onChangeOnly events) of
      Just a  -> Right a
      Nothing -> Left NoValue
  where
    onChangeOnly
      = case _ of
          OnChange a -> Just a
          _          -> Nothing

_mostRecentValue
  :: forall rawType niceType e
   . Getter' (PrismControl rawType niceType e) 
             (Either (EventError e) rawType) 
_mostRecentValue
  = _events <<< to (mostRecentValue)

---

_lastGoodValue
  :: forall rawType niceType e
   . Getter' (PrismControl rawType niceType e)
             (Either (EventError e) niceType)
_lastGoodValue
  = to outputGetter
  where
    outputGetter a
      = lastGoodValue (view _eitherFrom a) (view _events a)

lastGoodValue
  :: forall rawType niceType e
   . (rawType -> Either e niceType)
  -> ControlEvents rawType
  -> Either (EventError e) niceType
lastGoodValue eitherFrom events
  = case unwrap $ foldMap Last values of
      Just a -> Right a
      Nothing -> Left NoValue

  where
    values
      = map (toMaybes <<< tryDecode eitherFrom) events

    toMaybes a
      = case a of
             Right a' -> Just a'
             Left e   -> Nothing

tryDecode
  :: forall rawType niceType e
   . (rawType -> Either e niceType)
  -> Event rawType
  -> Either (EventError e) niceType
tryDecode eitherFrom event
  = case event of
      OnChange a -> wrapError (eitherFrom a)
      _          -> Left NoValue

---

getEitherOutput
  :: forall i e a
   . (i -> Either e a)
  -> ControlEvents i
  -> Either (EventError e) a
getEitherOutput eitherFrom events
  = case mostRecentValue events of
      Left e -> Left e
      Right a -> wrapError (eitherFrom a)

wrapError 
  :: forall e a
   . Either e a 
  -> Either (EventError e) a
wrapError 
  = left ValidationError 

_getEitherOutput
  :: forall rawType niceType e
   . Getter' (PrismControl rawType niceType e) (Either (EventError e) niceType) 
_getEitherOutput
  = to outputGetter
  where
    outputGetter a
      = getEitherOutput (view _eitherFrom a) (view _events a)

---

createEmpty 
  :: forall rawType niceType e
   . (niceType -> rawType)
  -> (rawType -> Either e niceType)
  -> PrismControl rawType niceType e
createEmpty to from
  = PrismControl
      { events:    mempty
      , eitherFrom: from
      , to:        to
      }

log 
  :: forall rawType niceType e
   . Event rawType
  -> PrismControl rawType niceType e
  -> PrismControl rawType niceType e
log a 
  = over _events (addEvent a)

addEvent :: forall a. a -> Array a -> Array a
addEvent a as 
  = (as <> (pure a))


