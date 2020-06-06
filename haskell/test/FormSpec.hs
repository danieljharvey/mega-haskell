module FormSpec where

import Form ()
import Test.Hspec

spec :: Spec
spec =
  describe "Form"
    $ it "Form returns a result"
    $ 1 `shouldBe` (1 :: Int)
