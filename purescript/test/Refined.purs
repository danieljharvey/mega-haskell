module Tests.Refined where

import Prelude (Unit, discard, negate, (<$>))
import Data.Either (Either(..))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Refined
import Data.Typelevel.Num.Reps (D1, D9)
import Data.Typelevel.Undefined (undefined)

tests :: Free TestF Unit
tests =
  suite "Refined" do
    test "Fail to validate a number that is over LessThan" do
      let ans = validate (undefined :: (LessThan D9)) 10.0 
      Assert.equal ans (Left RefinedError)
    test "Successfully validate number that is LessThan" do
      let ans = validate (undefined :: (LessThan D9)) 8.0
      Assert.equal ans (Right 8.0)
    test "Fail to validate a number that is under GreaterThan" do
      let ans = validate (undefined :: (GreaterThan D9)) 8.0 
      Assert.equal ans (Left RefinedError)
    test "Successfully validate number that over GreaterThan" do
      let ans = validate (undefined :: (GreaterThan D9)) 100.0
      Assert.equal ans (Right 100.0)
    test "Fails From" do
      let ans = validate (undefined :: (From D9)) 8.9 
      Assert.equal ans (Left RefinedError)
    test "Succeeds From" do
      let ans = validate (undefined :: (From D9)) 9.0
      Assert.equal ans (Right 9.0)
    test "Fails To" do
      let ans = validate (undefined :: (To D9)) 9.1 
      Assert.equal ans (Left RefinedError)
    test "Succeeds To" do
      let ans = validate (undefined :: (To D9)) 8.9
      Assert.equal ans (Right 8.9)
    test "Validates positive" do
      let ans = validate (undefined :: Positive) 10.0
      Assert.equal ans (Right 10.0)
    test "Validates not positive" do
      let ans = validate (undefined :: Positive) (-10.0)
      Assert.equal ans (Left RefinedError)
    test "And rejects correctly" do
      let ans = validate (undefined :: And (From D1) (To D9)) 11.0
      Assert.equal ans (Left RefinedError)
    test "And works correctly" do
      let ans = validate (undefined :: And (From D1) (To D9)) 8.0
      Assert.equal ans (Right 8.0)  
    test "Or rejects correctly" do
      let ans = validate (undefined :: Or (From D9) (To D1)) 8.0
      Assert.equal ans (Left RefinedError)
    test "Or works correctly" do
      let ans = validate (undefined :: Or (From D9) (To D1)) 10.0
      Assert.equal ans (Right 10.0)   
    test "Not rejects correctly" do
      let ans = validate (undefined :: Not (From D9)) 10.0
      Assert.equal ans (Left RefinedError)
    test "Not works correctly" do
      let ans = validate (undefined :: Not (From D9)) 8.0
      Assert.equal ans (Right 8.0) 
    test "Equal rejects correctly" do
      let ans = validate (undefined :: EqualTo D9) 8.0
      Assert.equal ans (Left RefinedError)
    test "Equal works correctly" do
      let ans = validate (undefined :: EqualTo D9) 9.0
      Assert.equal ans (Right 9.0)
    test "Whole thing works" do
      let ans = (refine 8.0 :: Either RefinedError (Refined Positive Number))
      Assert.equal ans (Right (Refined 8.0))
    test "We can get the value back out..." do
      let ans = (refine 8.0 :: Either RefinedError (Refined Positive Number))
      Assert.equal (unrefine <$> ans) (Right 8.0) 

