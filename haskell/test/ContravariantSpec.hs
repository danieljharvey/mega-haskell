module ContravariantSpec where

import           Contravariant
import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec =
  describe "Contravariant" $ do
    it "overThree" $
      getPreddy overThree 4 `shouldBe` True
    it "notOverThree" $
      getPreddy overThree 3 `shouldBe` False
    it "nameLength" $
      nameLength "" `shouldBe` 0
    it "nameLength 2" $
      nameLength "Dog" `shouldBe` 3
    it "nameIsOverThree 1" $
      nameIsOverThree "Lou" `shouldBe` False
    it "nameIsOverThree 2" $
      nameIsOverThree "Doug" `shouldBe` True
    it "personTooLong 1" $
      personTooLong steve `shouldBe` True
    it "personTooLong 2" $
      personTooLong lou `shouldBe` False
