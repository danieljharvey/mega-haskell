module FoldEvents where

import Prelude
 
import Data.Array (find) 
import Data.Foldable (foldMap, foldr) 
import Data.Lens (Prism', over, preview, prism', review) 
import Data.Lens.Getter (to)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Internal.Forget (Forget)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype, unwrap) 
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))

---

newtype PrismControl rawType niceType
  = PrismControl
      { events :: ControlEvents rawType
      , to     :: niceType -> rawType
      , from   :: rawType -> Maybe niceType
      }

derive instance newtypePrismControl 
  :: Newtype (PrismControl i a) _

---

newtype PrismDefaultControl rawType niceType
  = PrismDefaultControl
      { events  :: ControlEvents rawType
      , to      :: niceType -> rawType
      , from    :: rawType -> Maybe niceType
      , default :: niceType
      } 

derive instance newtypePrismDefaultControl 
  :: Newtype (PrismDefaultControl i a) _

---

newtype LensControl rawType niceType
  = LensControl
      { events  :: ControlEvents rawType
      , to      :: niceType -> rawType
      , from    :: rawType -> niceType
      }

derive instance newtypeLensControl 
  :: Newtype (LensControl i a) _

---

newtype LensDefaultControl rawType niceType
  = LensDefaultControl
      { events  :: ControlEvents rawType
      , to      :: niceType -> rawType
      , from    :: rawType -> niceType
      , default :: niceType
      }

derive instance newtypeLensDefaultControl 
  :: Newtype (LensDefaultControl i a) _

---

type ControlEvents rawType
  = Array (Event rawType)

---

events' 
  :: forall i p rawType r
   . Newtype i
      { events :: ControlEvents rawType
      | r
      }
  => Newtype i
      { events :: ControlEvents rawType
      | r
      }
  => Profunctor p 
  => Strong p 
  => p (ControlEvents rawType) (ControlEvents rawType) 
  -> p i i
events' = _Newtype <<< prop (SProxy :: SProxy "events")

---

to' 
  :: forall i p r a
   . Newtype i
      { to :: a
      | r
      }
  => Newtype i
      { to :: a
      | r
      }
  => Profunctor p 
  => Strong p 
  => p a a 
  -> p i i
to' = _Newtype <<< prop (SProxy :: SProxy "to")

---

from' 
  :: forall i p r a
   . Newtype i
      { from :: a
      | r
      }
  => Newtype i
      { from :: a
      | r
      }
  => Profunctor p 
  => Strong p 
  => p a a 
  -> p i i
from' = _Newtype <<< prop (SProxy :: SProxy "from")

---

default' 
  :: forall i p r a
   . Newtype i
      { default :: a
      | r
      }
  => Newtype i
      { default :: a
      | r
      }
  => Profunctor p 
  => Strong p 
  => p a a 
  -> p i i
default' = _Newtype <<< prop (SProxy :: SProxy "default")

--

class WithEvents a rawType where
  addEvent :: a -> Event rawType -> a

instance withEventsPrismControl :: WithEvents (PrismControl rawType a) rawType where
  addEvent prismControl event
    = over events' (\as -> as <> [event]) prismControl

instance withEventsPrismDefaultControl :: WithEvents (PrismDefaultControl rawType a) rawType where
  addEvent prismDefaultControl event
    = over events' (\as -> as <> [event]) prismDefaultControl

instance withEventsLensControl :: WithEvents (LensControl rawType a) rawType where
  addEvent lensControl event
    = over events' (\as -> as <> [event]) lensControl

instance withEventsLensDefaultControl :: WithEvents (LensDefaultControl rawType a) rawType where
  addEvent lensDefaultControl event
    = over events' (\as -> as <> [event]) lensDefaultControl

---

data Pet
  = Dog
  | Cat
  | Horse

derive instance genericPet :: Generic Pet _ 

instance showPet :: Show Pet where
  show = genericShow

data Event rawType
  = OnBlur
  | OnFocus
  | OnChange rawType

derive instance eqEvent :: (Eq internalType) => Eq (Event internalType)

hasFocus 
  :: forall i 
   . ControlEvents i 
  -> Boolean
hasFocus events 
  = foldr reducey false events
  where
    reducey
      = (\e t -> case e of
          OnFocus -> true
          OnBlur  -> false
          _       -> t) 

hasFocus' :: forall t173 t174 t176 t177 t182.
  Newtype t176
    { events :: Array (Event t174)
    | t173
    }
   => Forget t182 Boolean t177 -> Forget t182 t176 t176
hasFocus' = events' <<< to (hasFocus)

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

mostRecentValue' :: forall t187 t188 t190 t191 t196.
  Newtype t190
    { events :: Array (Event t188)
    | t187
    }
   => Forget t196 (Maybe t188) t191 -> Forget t196 t190 t190
mostRecentValue'
  = events' <<< to (mostRecentValue)

getOutput
  :: forall i a
   . (i -> Maybe a)
  -> ControlEvents i
  -> Maybe a
getOutput maybeFrom events
  = mostRecentValue events >>= maybeFrom



outputIsValid
  :: forall i a 
   . (i -> Maybe a) 
  -> ControlEvents i
  -> Boolean
outputIsValid maybeFrom events
  = isJust (getOutput maybeFrom events)

hasBlurred 
  :: forall i
   . (Eq i) 
  => ControlEvents i
  -> Boolean
hasBlurred events
  = isJust (find ((==) OnBlur) events)

hasHadFocus 
  :: forall i 
   . (Eq i) 
  => ControlEvents i 
  -> Boolean
hasHadFocus events
  = isJust (find ((==) OnFocus) events)

petSelector :: PrismControl String Pet
petSelector 
  = PrismControl
      { events:  []
      , from:    preview pet'
      , to:      review pet'
      }

pet' :: Prism' String Pet
pet' = prism' toPet fromPet
  where
     fromPet = case _ of 
                 "Dog"   -> Just Dog
                 "Cat"   -> Just Cat
                 "Horse" -> Just Horse
                 _       -> Nothing
     toPet   = show
