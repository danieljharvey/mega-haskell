module ValidationSpec where

import Test.Hspec
import Validation ()

spec :: Spec
spec =
  describe "Validation"
    $ it "empty test"
    $ 1 `shouldBe` (1 :: Int)
