module SemilatticeSpec where

import Data.Semilattice.Join
import Semilattice
import Test.Hspec

spec :: Spec
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
