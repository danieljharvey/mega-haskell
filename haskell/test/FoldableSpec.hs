module FoldableSpec where

import Foldable
import Test.Hspec

spec :: Spec
spec =
  describe "Foldable" $ do
    it "added" $
      added `shouldBe` 10
    it "maxNo" $
      maxNo `shouldBe` 4
    it "addTwo" $
      addTwo `shouldBe` 10
    it "twentyFour" $
      twentyFour `shouldBe` 24
    it "allOfThem" $
      allOfThem `shouldBe` True
    it "notAll" $
      notAll `shouldBe` False
