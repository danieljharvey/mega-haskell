module SemigroupSpec where

import           Control.Exception (evaluate)
import           Semigroup
import           Test.Hspec
import           Test.QuickCheck   hiding (NonEmpty)

-- spec :: IO ()
spec =
  describe "Semigroups" $ do
    describe "list" $ do
      it "combines lists with semigroup" $
        thirdList `shouldBe` [1,2,3,4,5,6]
      it "uses ++" $
        [1,2,3] ++ [4,5,6] `shouldBe` [1,2,3,4,5,6]
    describe "string" $
      it "uses semigroup" $
        thirdString `shouldBe` "GreatStuff"
    describe "MySum" $ do
      it "adds two numbers in a very silly way" $
        ten `shouldBe` 10
      it "does not matter what order the elements are in" $
        ten `shouldBe` anotherTen
    describe "MyProduct" $ do
      it "multiplies two numbers in a very silly way" $
        sixtySix `shouldBe` 66
      it "does not matter what order the elements are in" $
        sixtySix `shouldBe` anotherSixtySix
    describe "MyString" $ do
      it "allTheThings" $
        allTheThings `shouldBe` "AllTheThings"
      it "otherAllTheThings" $
        otherAllTheThings `shouldBe` "AllTheThings"
    describe "MyMaybe" $ do
      it "nah" $
        nah `shouldBe` Nope
      it "first" $
        first `shouldBe` Yeah "Totally"
      it "second" $
        second `shouldBe` Yeah "Great"
      it "both" $
        both `shouldBe` Yeah "TotallyGreat"
