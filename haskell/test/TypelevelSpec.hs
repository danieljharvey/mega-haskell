module TypelevelSpec where

import           Control.Exception      (evaluate)
import           Control.Monad.Identity
import           Test.Hspec
import           Test.QuickCheck        hiding (NonEmpty)
import           Typelevel

-- spec :: IO ()
spec =
  describe "Typelevel" $
    describe "what" $
      it "is going on" $
          "dog" `shouldBe` "dog"
