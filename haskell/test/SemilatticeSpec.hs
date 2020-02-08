module SemilatticeSpec where

import Control.Exception (evaluate)
import Data.Semilattice.Join
import Semilattice
import Test.Hspec
import Test.QuickCheck hiding (NonEmpty)

-- spec :: IO ()
spec =
  describe "Semilattics" $ do
    describe "Haircut" $ do
      it "Idempotent 1" $ do
        White \/ White `shouldBe` White
        Black \/ Black `shouldBe` Black
      it "Commutative" $
        DarkGrey \/ Black `shouldBe` Black \/ DarkGrey
      it "Associative" $
        DarkGrey \/ (Grey \/ LightGrey) `shouldBe` (DarkGrey \/ Grey) \/ LightGrey
