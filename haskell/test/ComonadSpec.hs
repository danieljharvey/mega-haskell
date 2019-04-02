module ComonadSpec where

import           Comonad
import           Control.Comonad.Store
import           Control.Exception     (evaluate)
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Which Comonad" $ do
    it "currentPosition" $
        currentPosition `shouldBe` This
    it "somethingElse" $
        somethingElse `shouldBe` "that other thing"
    it "swapped" $
        swapped `shouldBe` "that other thing"
    it "itsThat" $
        itsThat `shouldBe` That
    it "nowItsWho" $
        nowItsWho `shouldBe` Who
    it "what" $
        what `shouldBe` Just "this thing"
    it "otherWhat" $
        otherWhat `shouldBe` Just "that other thing"
    it "otherOtherWhat" $
        otherOtherWhat `shouldBe` Nothing
  describe "Battenburg" $ do
    it "seems ok" $
        getBattenType (First, First) `shouldBe` Pink
    it "seems ok" $
        getBattenType (Middle, First) `shouldBe` Other
  describe "Grid" $ do
    it "Gets 0,0" $
      getGridItem startGrid (0,0) `shouldBe` True
    it "Gets 2,2" $
      getGridItem startGrid (2,2) `shouldBe` False
    it "Gets 5,5" $
      getGridItem startGrid (3,3) `shouldBe` True
    it "Deals with underflow" $
      getGridItem startGrid (-10, -10) `shouldBe` False
    it "Deals with overflow" $
      getGridItem startGrid (10, 10) `shouldBe` False
    it "firstItem is 1" $
      firstItem `shouldBe` False
    it "secondItem is 5" $
      secondItem `shouldBe` 4
    it "thirdItem is 3" $
      thirdItem `shouldBe` 2
    it "fourthItem is 0" $
      fourthItem `shouldBe` 2
  describe "MyNonEmpty" $ do
    it "Sums the list using extend" $
      sumList `shouldBe` 28 :| [27, 25, 22, 18, 13, 7]
    it "Products the list using extend" $
      productList `shouldBe` 5040 :| [5040,2520,840,210,42,7]
