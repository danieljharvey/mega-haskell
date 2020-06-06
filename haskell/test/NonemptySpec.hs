module NonemptySpec where

import Nonempty
import Test.Hspec

spec :: Spec
spec =
  describe "NonEmpty"
    $ it "third"
    $ third `shouldBe` NonEmpty 1 [2, 3, 4, 5, 6, 7, 8]
