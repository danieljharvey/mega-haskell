module BifunctorSpec where

import Bifunctor
import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck hiding (NonEmpty)

-- spec :: IO ()
spec =
  describe "Bifunctor" $ do
    describe "Regular function" $ do
      it "Maps over b" $
        biggerNumber `shouldBe` That 69
      it "Ignores a" $
        doesNothing `shouldBe` This "Egg"
    describe "real bifunctor" $ do
      it "maps over a" $
        delicious `shouldBe` This "The Egg was delicious!"
      it "maps over b" $
        doesWork `shouldBe` biggerNumber
    describe "Tuple bimap"
      $ it "Maps over both"
      $ oneBestSeller `shouldBe` (101, "Dalmations")
