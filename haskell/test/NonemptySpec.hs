module NonemptySpec where

import Control.Exception (evaluate)
import Nonempty
import Test.Hspec
import Test.QuickCheck hiding (NonEmpty)

-- spec :: IO ()
spec =
  describe "NonEmpty"
    $ it "third"
    $ third `shouldBe` NonEmpty 1 [2, 3, 4, 5, 6, 7, 8]
