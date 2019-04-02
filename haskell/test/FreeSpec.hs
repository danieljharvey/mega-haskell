module FreeSpec where

import qualified Control.Monad.State as St
import           Free
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec =
  describe "Free" $
    describe "Applicative" $
      it "Doesn't break with second Nope" $
        St.runState (interpretState fetchAction) initialState
          `shouldBe` ((), State { string = Just "test item"
                                , url = "http://internetisverymuchmybusiness.com"
                                , loading = False
                                })
