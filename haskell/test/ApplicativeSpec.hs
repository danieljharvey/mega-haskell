module ApplicativeSpec where

import Applicative
import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "intuitions" $ do
    it "listOfLists" $
      listOfLists `shouldBe` [[0, 1, 2], [1, 2, 3], [2, 3, 4]]
    it "bigList" $
      bigList `shouldBe` [0, 1, 2, 1, 2, 3, 2, 3, 4]
    it "applicativeList" $
      applicativeList `shouldBe` [0, 1, 2, 1, 2, 3, 2, 3, 4]
  describe "Applicative" $ do
    it "Maps correctly" $
      two `shouldBe` CalcFace ["1"] 2
    it "Applies correctly" $
      oneAddOneAddOne `shouldBe` CalcFace ["1", "add 1", "add 1"] 3
    it "Monadically applies" $
      oneAddThreeAddThreeMonadically `shouldBe` CalcFace ["add 3"] 7
    it "Quickchecks functor identity" $
      quickCheck calcFaceFunctorIdentity
    it "Quickchecks applicative identity" $
      quickCheck calcFaceApplicativeIdentity
  describe "showCalculation" $ do
    it "Shows one" $
      showCalculation one `shouldBe` "1 equals 1"
    it "Shows broken functor version" $
      showCalculation two `shouldBe` "1 equals 2"

instance Arbitrary a => Arbitrary (CalcFace a) where
  arbitrary = do
    a <- arbitrary
    xs <- arbitrary
    return $ CalcFace xs a

calcFaceFunctorIdentity :: CalcFace String -> Bool
calcFaceFunctorIdentity cf = fmap id cf == cf

calcFaceApplicativeIdentity :: CalcFace String -> Bool
calcFaceApplicativeIdentity v = (pure id <*> v) == v
