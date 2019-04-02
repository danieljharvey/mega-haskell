module ValidationSpec where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck
import           Validation

-- spec :: IO ()
spec =
    describe "Validation" $
        it "empty test" $
            1 `shouldBe` 1

