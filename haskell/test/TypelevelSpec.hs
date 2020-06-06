module TypelevelSpec where

import Test.Hspec
import Typelevel ()

spec :: Spec
spec =
  describe "Typelevel"
    $ describe "what"
    $ it "is going on"
    $ "dog" `shouldBe` "dog"
