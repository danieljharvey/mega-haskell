module FoldEvents where

import Prelude
 
import Data.Array (find) 
import Data.Foldable (foldMap, foldr) 
import Data.Lens (Prism', preview, prism', review) 
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe.Last (Last(..))
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Control rawType a
  = Control
      { events        :: Array (Event rawType)
      , to            :: a -> rawType
      , from          :: rawType -> Maybe a
      }

data Pet
  = Dog
  | Cat
  | Horse

derive instance genericPet :: Generic Pet _ 

instance showPet :: Show Pet where
  show = genericShow

data Event rawType
  = OnClick
  | OnBlur
  | OnFocus
  | OnChange rawType

derive instance eqEvent :: (Eq internalType) => Eq (Event internalType)

hasFocus 
  :: forall i a
   . Control i a 
  -> Boolean
hasFocus (Control { events }) 
  = foldr reducey false events
  where
    reducey
      = (\e t -> case e of
          OnFocus -> true
          OnBlur  -> false
          _       -> t) 

mostRecentValue 
  :: forall i a
   . Control i a 
  -> Maybe i
mostRecentValue (Control { events })
  = unwrap $ foldMap Last (map onChangeOnly events)
  where
    onChangeOnly
      = case _ of
          OnChange a -> Just a
          _          -> Nothing

getOutput
  :: forall i a
   . Control i a
  -> Maybe a
getOutput control@(Control { from })
  = mostRecentValue control >>= from

outputIsValid
  :: forall i a
   . Control i a
  -> Boolean
outputIsValid
  = isJust <<< getOutput

hasBlurred 
  :: forall i a
   . (Eq i) 
  => Control i a 
  -> Boolean
hasBlurred (Control { events })
  = isJust (find ((==) OnBlur) events)

hasHadFocus 
  :: forall i a
   . (Eq i) 
  => Control i a 
  -> Boolean
hasHadFocus (Control { events })
  = isJust (find ((==) OnFocus) events)

petSelector :: Control String Pet
petSelector 
  = Control
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
