module TestingSpec where

import Control.Monad.Identity
import Test.Hspec
import Testing

spec :: Spec
spec =
  describe "Testing" $ do
    describe "time" $ do
      it "gets the hour of test data" $
        getHour baseTestTime `shouldBe` 0
      it "gets the hour of start lunch test data" $
        getHour lunchTestTime `shouldBe` 12
      it "gets the hour of after lunch test data" $
        getHour endLunchTestTime `shouldBe` 14
    describe "testable" $ do
      it "runs isItLunchTime2" $
        testableLunch (pure baseTestTime) `shouldBe` Identity False
      it "runs isItLunchTime2" $
        testableLunch (pure lunchTestTime) `shouldBe` Identity True
    describe "testClassyLunch"
      $ it "Uses the identity one"
      $ testClassyLunch `shouldBe` Identity True
