module FormSpec where

import Control.Exception (evaluate)
import Form
import Test.Hspec
import Test.QuickCheck hiding (NonEmpty)

-- spec :: IO ()
spec =
  describe "Form"
    $ it "Form returns a result"
    $ 1 `shouldBe` 1
