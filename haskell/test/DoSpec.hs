module DoSpec where

import Control.Exception (evaluate)
import DoNotation
import Test.Hspec
import Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "DoNotation" $ do
    it "Head function with item" $ do
      safeHead [1, 2, 3] `shouldBe` Just 1
    it "Head function with no item" $ do
      safeHead ([] :: [Int]) `shouldBe` Nothing
    it "safeHeadTwice" $ do
      safeHeadTwice [[1]] `shouldBe` Just 1
    it "safeHeadTwice2" $ do
      safeHeadTwice2 [[1]] `shouldBe` Just 1
    it "safeHeadTwiceShort" $ do
      safeHeadTwiceShort [[1]] `shouldBe` Just 1
