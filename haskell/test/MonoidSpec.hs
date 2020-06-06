module MonoidSpec where

import Monoid
import Test.Hspec

spec :: Spec
spec = describe "Monoids" $ do
  describe "List" $ do
    it "great" $ do
      great `shouldBe` [1, 2, 3, 4, 5, 6]
    it "combineList" $ do
      combineList1 [[1, 2, 3], [4, 5, 6]] `shouldBe` ([1, 2, 3, 4, 5, 6] :: [Int])
    it "combineList empty" $ do
      combineList1 [] `shouldBe` ([] :: [Int])
  it "calculates 10 with MySum" $
    ten `shouldBe` 10
  it "calculates 66 with MyProduct" $
    sixtySix `shouldBe` 66
