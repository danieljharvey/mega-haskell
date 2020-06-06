{-# OPTIONS_GHC -fno-warn-orphans #-}

module FunctorSpec where

import Functor
import Test.Hspec
import Test.QuickCheck hiding (NonEmpty)

spec :: Spec
spec =
  describe "Functor" $ do
    it "questionAdd john" $
      questionAdd john `shouldBe` Yeah "John???"
    it "questionAdd nah" $
      questionAdd nope `shouldBe` Nah
    it "exclaimAdd john" $
      exclaimAdd john `shouldBe` Yeah "John!"
    it "exclaimAdd nah" $
      exclaimAdd nope `shouldBe` Nah
    it "exclaims" $
      exclaim "Horse" `shouldBe` "Horse!!!!!!!!!!!!"
    it "capitalses" $
      capitalise "Horse" `shouldBe` "HORSE"
    it "veryJohn" $
      veryJohn `shouldBe` Yeah "John!!!!!!!!!!!!"
    it "stillNope" $
      stillNope `shouldBe` Nah
    it "identity" $
      identityLaw (Yeah "john") `shouldBe` True
    it "compositionLaw" $
      compositionLaw (Yeah "mate") `shouldBe` True
    it "Checks identity law" $
      quickCheck perhapsFunctorIdentity
    it "Checks composition law" $
      quickCheck perhapsFunctorComposition
    it "composes" $
      fmap (capitalise . exclaim) (Yeah "Bruce") `shouldBe` Yeah "BRUCE!!!!!!!!!!!!"
    it "broken identity" $
      identityLaw2 (Yerp "john") `shouldBe` False
    it "broken compositionLaw" $
      compositionLaw2 (Yerp "mate") `shouldBe` True

perhapsFunctorIdentity :: Perhaps String -> Bool
perhapsFunctorIdentity cf = fmap id cf == cf

perhapsFunctorComposition :: Perhaps String -> Bool
perhapsFunctorComposition j = fmap (capitalise . exclaim) j == fmap capitalise (fmap exclaim j)

instance Arbitrary a => Arbitrary (Perhaps a) where
  arbitrary = Yeah <$> arbitrary
