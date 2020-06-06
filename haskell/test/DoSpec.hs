module DoSpec where

import DoNotation
import Test.Hspec

spec :: Spec
spec = do
  describe "DoNotation" $ do
    it "Head function with item" $ do
      safeHead [1, 2, 3] `shouldBe` Just (1 :: Int)
    it "Head function with no item" $ do
      safeHead ([] :: [Int]) `shouldBe` Nothing
    it "safeHeadTwice" $ do
      safeHeadTwice [[1]] `shouldBe` Just (1 :: Int)
    it "safeHeadTwice2" $ do
      safeHeadTwice2 [[1]] `shouldBe` Just (1 :: Int)
    it "safeHeadTwiceShort" $ do
      safeHeadTwiceShort [[1]] `shouldBe` Just (1 :: Int)
