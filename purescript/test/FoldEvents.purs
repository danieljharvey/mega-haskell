module Tests.FoldEvents where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Data.Lens (Lens', over, view) 
import Data.Lens.Record (prop)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))

import FoldEvents (Event(..), PrismControl, createEmpty, currentValue, hasFocus, lastGoodValue, logEvent, logValue)

tests :: Free TestF Unit
tests = do
  suite "FoldEvents" do
    test "Returns Nothing when no most recent value" do
      Assert.equal (currentValue petValue) (Left (UnknownPet "oh"))
    test "Returns Just Dog when no most recent value" do
       let petWithValue = logValue "Dog" petValue 
       Assert.equal (currentValue petWithValue) (Right Dog)
    test "Finds last good value" do
       Assert.equal (lastGoodValue petValue) (Just Dog)
    test "Knows that we are not currently focused" do
       Assert.equal (hasFocus petValue) false
    test "Works nested in a record" do
       Assert.equal (hasFocus $ view _val testRecord) false
    test "Updates work nested in a record" do
       Assert.equal (hasFocus $ view _val testRecordWithVal) true

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

---


addTestValues 
  :: PrismControl String Pet PetError
  -> PrismControl String Pet PetError
addTestValues  
  = (logEvent OnBlur)
       <<< (logEvent OnFocus)
       <<< (logValue "oh")
       <<< (logValue "Dog")
       <<< (logValue "Doge")

petValue :: PrismControl String Pet PetError
petValue = addTestValues (createEmpty "" toPet fromPet)

type TestRecord
  = { val  :: PrismControl String Pet PetError
    , val2 :: PrismControl String Pet PetError
    }

testRecord :: TestRecord
testRecord 
  = { val:  createEmpty "" toPet fromPet
    , val2: createEmpty "" toPet fromPet
    }

_val :: Lens' TestRecord (PrismControl String Pet PetError)
_val = prop (SProxy :: SProxy "val")

_val2 :: Lens' TestRecord (PrismControl String Pet PetError)
_val2 = prop (SProxy :: SProxy "val2")

valIsFocused 
  :: TestRecord 
  -> Boolean
valIsFocused = hasFocus <<< view _val

testRecordWithVal :: TestRecord
testRecordWithVal 
  = over _val (logEvent OnFocus) testRecord


