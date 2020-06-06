module ProfunctorSpec where

import Profunctor
import Test.Hspec

spec :: Spec
spec =
  describe "Bifunctor" $ do
    it "One" $
      one `shouldBe` 6
    it "Two" $
      two `shouldBe` [Egg, Egg, Egg]
    it "Three" $
      three `shouldBe` 5
    it "Four" $
      four `shouldBe` "3"
