module Tests.FoldEvents where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import FoldEvents (Event(..), MiniStore, allGoodValues, createEmpty, currentValue, hasFocus, lastGoodValue, logEvent, logValue)

tests :: Free TestF Unit
tests = do
  suite "FoldEvents" do
    test "Returns Nothing when no most recent value" do
      Assert.equal (currentValue petValue) (Left (UnknownPet "oh"))
    test "Returns Just Dog when no most recent value" do
      let
        petWithValue = logValue "Dog" petValue
      Assert.equal (currentValue petWithValue) (Right Dog)
    test "Finds last good value" do
      Assert.equal (lastGoodValue petValue) (Just Dog)
    test "Knows that we are not currently focused" do
      Assert.equal (hasFocus petValue) false
    test "Gets allGoodValues" do
      Assert.equal [ Cat, Dog ] (allGoodValues (logValue "Cat" petValue))

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
  "Dog" -> Right Dog
  "Cat" -> Right Cat
  "Horse" -> Right Horse
  a -> Left (UnknownPet a)

toPet :: Pet -> String
toPet = show

---
addTestValues ::
  MiniStore String Pet PetError ->
  MiniStore String Pet PetError
addTestValues =
  (logEvent OnBlur)
    <<< (logEvent OnFocus)
    <<< (logValue "oh")
    <<< (logValue "Dog")
    <<< (logValue "Doge")

petValue :: MiniStore String Pet PetError
petValue = addTestValues (createEmpty "" toPet fromPet)

---

type SampleData
  = { firstThing :: Pet
    , secondThing :: Pet
    }

type TwoDifferentPets = Tuple Pet

data SamePets = SamePets

{-
combined :: MiniStore SampleData TwoDifferentPets SamePets
combined = createEmpty 
    { firstThing: Dog, secondThing: Dog } 

-}
