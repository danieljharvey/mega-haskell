module CombinatorsSpec where

import Combinators ()
import Test.Hspec

spec :: Spec
spec = do
  describe "Combinators" $ do
    it "Seem fine" $
      True `shouldBe` True
