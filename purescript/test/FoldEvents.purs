module Tests.FoldEvents where

import Prelude
import Data.Either (Either(..))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Data.Lens (Lens', over, view) 
import Data.Lens.Record (prop)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))


import FoldEvents (Event(..), EventError(..), PrismControl, _getEitherOutput, _hasFocus, _lastGoodValue, createEmpty, log) 

tests :: Free TestF Unit
tests = do
  suite "FoldEvents" do
    test "Returns Nothing when no most recent value" do
      Assert.equal (view _getEitherOutput petValue) (Left (ValidationError
                   (UnknownPet "oh")))
    test "Returns Just Dog when no most recent value" do
      Assert.equal (view _getEitherOutput petWithValue) (Right Dog)
    test "Finds last good value" do
       Assert.equal (view _lastGoodValue petValue) (Right Dog)
    test "Knows that we are not currently focused" do
       Assert.equal (view _hasFocus petValue) false
    test "Works nested in a record" do
       Assert.equal (valIsFocused testRecord) false
    test "Updates work nested in a record" do
       Assert.equal (valIsFocused testRecordWithVal) true

data Pet
  = Dog
  | Cat
  | Horse

derive instance genericPet :: Generic Pet _ 
derive instance eqPet :: Eq Pet

instance showPet :: Show Pet where
  show = genericShow

data PetError
  = UnknownPet String

derive instance eqPetError :: Eq PetError
instance showPetError :: Show PetError where
  show (UnknownPet s) = "Unknown Pet: " <> s


fromPet :: String -> Either PetError Pet
fromPet = case _ of 
   "Dog"   -> Right Dog
   "Cat"   -> Right Cat
   "Horse" -> Right Horse
   a       -> Left (UnknownPet a)

toPet :: Pet -> String
toPet   = show

testing 
  :: PrismControl String Pet PetError
  -> PrismControl String Pet PetError
testing  = (log OnBlur)
       <<< (log OnFocus)
       <<< (log (OnChange "oh"))
       <<< (log (OnChange "Dog"))
       <<< (log (OnChange "Doge"))

petValue :: PrismControl String Pet PetError
petValue = testing (createEmpty toPet fromPet)

petWithValue :: PrismControl String Pet PetError
petWithValue = log (OnChange "Dog") petValue

type TestRecord
  = { val :: PrismControl String Pet PetError
    }

testRecord :: TestRecord
testRecord = { val: petValue }

_val :: Lens' TestRecord (PrismControl String Pet PetError)
_val = prop (SProxy :: SProxy "val")

valIsFocused 
  :: TestRecord 
  -> Boolean
valIsFocused = view (_val <<< _hasFocus) 

testRecordWithVal :: TestRecord
testRecordWithVal = over _val (log OnFocus) testRecord
