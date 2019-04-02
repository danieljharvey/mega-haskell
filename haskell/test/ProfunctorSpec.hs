module ProfunctorSpec where

import           Control.Exception (evaluate)
import           Profunctor
import           Test.Hspec
import           Test.QuickCheck   hiding (NonEmpty)

-- spec :: IO ()
spec
  = describe "Bifunctor" $ do
      it "One" $
        one `shouldBe` 6
      it "Two" $
        two `shouldBe` [Egg, Egg, Egg]
      it "Three" $
        three `shouldBe` 5
      it "Four" $
        four `shouldBe` "3"
