module Tests.FoldEvents where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import FoldEvents (ControlEvents, Event(..), PrismControl, createEmpty, getMaybeOutput', hasFocus', log)
 
 
import Data.Lens (Lens', over, view) 
 
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Record (prop)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
 
import Data.Symbol (SProxy(..))



tests :: Free TestF Unit
tests = do
  suite "FoldEvents" do
    test "Returns Nothing when no most recent value" do
      Assert.equal (view getMaybeOutput' petValue)  Nothing
    test "Returns Just Dog when no most recent value" do
      Assert.equal (view getMaybeOutput' petWithValue) (Just Dog)



data Pet
  = Dog
  | Cat
  | Horse

derive instance genericPet :: Generic Pet _ 
derive instance eqPet :: Eq Pet

instance showPet :: Show Pet where
  show = genericShow

pet' :: Prism' String Pet
pet' = prism' toPet fromPet
  where
     fromPet = case _ of 
                 "Dog"   -> Just Dog
                 "Cat"   -> Just Cat
                 "Horse" -> Just Horse
                 _       -> Nothing
     toPet   = show

testing 
  :: forall r
   . { events :: ControlEvents String | r }
  -> { events :: ControlEvents String | r }
testing  = (log OnFocus)
       <<< (log OnBlur)
       <<< (log (OnChange "oh"))
       <<< (log (OnChange "Dog"))
       <<< (log (OnChange "Doge"))

petValue :: PrismControl String Pet
petValue = testing (createEmpty pet')

petWithValue :: PrismControl String Pet
petWithValue = log (OnChange "Dog") petValue

isFocused :: Boolean
isFocused = view hasFocus' petValue
-- false

type TestRecord
  = { val :: PrismControl String Pet
    }

testRecord :: TestRecord
testRecord = { val: petValue }

_val :: Lens' TestRecord (PrismControl String Pet)
_val = prop (SProxy :: SProxy "val")

valIsFocused 
  :: TestRecord 
  -> Boolean
valIsFocused = view (_val <<< hasFocus') 

logWithVal :: TestRecord
logWithVal = over _val (log OnFocus) testRecord

no :: Boolean
no = valIsFocused testRecord
-- false

yes :: Boolean
yes = valIsFocused logWithVal
-- true