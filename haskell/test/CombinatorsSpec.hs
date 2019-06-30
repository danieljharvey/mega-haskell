module CombinatorsSpec where

import           Combinators
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Combinators" $ do
    it "Seem fine" $
      True `shouldBe` True
