module FoldEvents where

import Prelude
 
import Data.Array (find) 
import Data.Foldable (foldMap, foldr) 
import Data.Lens (Getter', Lens', over, preview, view) 
import Data.Lens.Getter (to) 
import Data.Lens.Prism (Prism', review)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Maybe.Last (Last(..))
import Data.Newtype (unwrap) 
import Data.Symbol (SProxy(..))

---

type PrismControl rawType niceType
  = { events    :: ControlEvents rawType
    , to        :: niceType -> rawType
    , maybeFrom :: rawType -> Maybe niceType
    }

---

type PrismDefaultControl rawType niceType
  = { events    :: ControlEvents rawType
    , to        :: niceType -> rawType
    , maybeFrom :: rawType -> Maybe niceType
    , default   :: niceType
    } 

---

type ControlEvents rawType
  = Array (Event rawType)

---

events' 
  :: forall rawType r
   . Lens' { events :: ControlEvents rawType | r } (ControlEvents rawType)
events' = prop (SProxy :: SProxy "events")

---

to' 
  :: forall niceType rawType r
   . Lens' { to :: (niceType -> rawType) | r } (niceType -> rawType)
to' = prop (SProxy :: SProxy "to")

---

from' 
  :: forall niceType rawType r
   . Lens' { from :: (rawType -> niceType) | r } (rawType -> niceType)
from' = prop (SProxy :: SProxy "from")

---

maybeFrom'
  :: forall niceType rawType r
   . Lens' { maybeFrom :: (rawType -> Maybe niceType) | r } (rawType -> Maybe niceType)
maybeFrom'
  = prop (SProxy :: SProxy "maybeFrom")
---

default'
  :: forall niceType r
   . Lens' { default :: niceType | r } niceType
default' = prop (SProxy :: SProxy "default")

--

data Event rawType
  = OnBlur
  | OnFocus
  | OnChange rawType

derive instance eqEvent :: (Eq internalType) => Eq (Event internalType)

---

hasFocus' 
  :: forall rawType r
   . Getter' { events :: ControlEvents rawType | r } Boolean
hasFocus' = events' <<< to (hasFocus)
  where
    hasFocus 
      = foldr reducey false 
      where
        reducey
          = (\e t -> case e of
              OnFocus -> true
              OnBlur  -> false
              _       -> t) 

---

hasBlurred' 
  :: forall rawType r
   . Eq rawType
  => Getter' { events :: ControlEvents rawType | r } Boolean
hasBlurred' = events' <<< to (isJust <<< find ((==) OnBlur))

---

hasHadFocus' 
  :: forall rawType r
   . Eq rawType
  => Getter' { events :: ControlEvents rawType | r } Boolean
hasHadFocus' = events' <<< to (isJust <<< find ((==) OnFocus))

---

mostRecentValue 
  :: forall i 
   . ControlEvents i 
  -> Maybe i
mostRecentValue events
  = unwrap $ foldMap Last (map onChangeOnly events)
  where
    onChangeOnly
      = case _ of
          OnChange a -> Just a
          _          -> Nothing

mostRecentValue'
  :: forall rawType r
   . Getter' { events :: ControlEvents rawType | r} (Maybe rawType) 
mostRecentValue'
  = events' <<< to (mostRecentValue)

---

getMaybeOutput
  :: forall i a
   . (i -> Maybe a)
  -> ControlEvents i
  -> Maybe a
getMaybeOutput maybeFrom events
  = mostRecentValue events >>= maybeFrom

getMaybeOutput'
  :: forall rawType niceType r
   . Getter' { events :: ControlEvents rawType
             , maybeFrom :: (rawType -> Maybe niceType) 
             | r
             }
     (Maybe niceType) 
getMaybeOutput'
  = to outputGetter
  where
    outputGetter a
      = getMaybeOutput (view maybeFrom' a) (view events' a)

---

maybeOutputIsValid
  :: forall i a 
   . (i -> Maybe a) 
  -> ControlEvents i
  -> Boolean
maybeOutputIsValid maybeFrom events
  = isJust (getMaybeOutput maybeFrom events)

maybeOutputIsValid' 
  :: forall rawType niceType r
   . Getter' { events :: ControlEvents rawType 
             , maybeFrom :: (rawType -> Maybe niceType)
             | r 
             } Boolean
maybeOutputIsValid' 
  = to isValid
  where
    isValid a
      = maybeOutputIsValid (view maybeFrom' a) (view events' a)

---

getOutput'
  :: forall rawType niceType r
   . Getter' { events    :: ControlEvents rawType
             , maybeFrom :: (rawType -> Maybe niceType)
             , default   :: niceType
             | r
             } niceType
getOutput'
  = to getOutput
  where
    getOutput a
      = fromMaybe (view default' a) 
                  (getMaybeOutput (view maybeFrom' a) (view events' a))

---

createEmpty 
  :: forall rawType niceType
   . Prism' rawType niceType
  -> PrismControl rawType niceType
createEmpty prism''
  = { events: []
    , maybeFrom: preview prism''
    , to:        review prism''
    }

createWithDefault
  :: forall rawType niceType
   . Prism' rawType niceType
  -> niceType
  -> PrismDefaultControl rawType niceType
createWithDefault prism'' def
  = { events: []
    , maybeFrom: preview prism''
    , to:        review prism''
    , default:   def
    }

log 
  :: forall rawType r
   . Event rawType
  -> { events :: ControlEvents rawType | r }
  -> { events :: ControlEvents rawType | r }
log a 
  = over events' (addEvent a)

addEvent :: forall a. a -> Array a -> Array a
addEvent a as 
  = (as <> (pure a))


---- sample

