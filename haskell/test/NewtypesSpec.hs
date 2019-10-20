module NewtypesSpec where

import Control.Exception (evaluate)
import Newtypes
import Test.Hspec
import Test.QuickCheck hiding (NonEmpty)

spec = describe "Newtypes" $ do
  it "Makes Frank" $
    getDog frank `shouldBe` "Frank"
  it "wraps and unwraps" $
    itsTheSame `shouldBe` True
  it "Calculates salary the stupid way" $
    calculateSalaryBad (-100) `shouldBe` -100000
  it "Stops me calculating negative salary" $
    calculateSalaryBetter (-100) `shouldBe` Nothing
  it "Lets me calculating valid salary" $
    calculateSalaryBetter 100 `shouldBe` Just 100000
  it "Cannot make a PositiveNum of -1" $
    makePositiveNum (-1) `shouldBe` Nothing
  it "Successfully makes a PositiveNum of 1" $
    makePositiveNum 1 `shouldBe` Just (PositiveNum 1)
  it "Calculates salary" $
    calculateSalary (PositiveNum 12) `shouldBe` 12000
  it "Calculates salary with functor instance" $
    calculateSalary2 (PositiveNum 13) `shouldBe` (PositiveNum 13000)
  it "Calculates salary with functor instance and unwraps" $
    calculateSalary3 (PositiveNum 14) `shouldBe` 14000
