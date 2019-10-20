module ArrowSpec where

import Arrow
import Test.Hspec

-- spec :: IO ()
spec =
  describe "Arrow"
    $ it "Combines the pets"
    $ pairOfPets `shouldBe` (Dog, Horse)
