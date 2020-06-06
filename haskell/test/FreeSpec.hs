module FreeSpec where

import qualified Control.Monad.State as St
import Free
import Test.Hspec

spec :: Spec
spec =
  describe "Free"
    $ describe "Applicative"
    $ it "Doesn't break with second Nope"
    $ St.runState (interpretState fetchAction) initialState
      `shouldBe` ( (),
                   State
                     { string = Just "test item",
                       url = Url "http://internetisverymuchmybusiness.com",
                       loading = False
                     }
                 )
