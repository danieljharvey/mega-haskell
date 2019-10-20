module EitherSpec where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Identity hiding (fail)
import Control.Monad.Zip
import Data.Bifunctor
import Data.Semigroup
import Either
import Test.Hspec
import Prelude hiding (Either (..), fail)

-- spec :: IO ()
spec =
  describe "Either" $ do
    it "Either functor with Right" $
      fmap (+ 1) (Right (1 :: Int)) `shouldBe` (Right 2 :: Either Int Int)
    it "Either functor with Left" $
      fmap (+ 1) (Left (1 :: Int)) `shouldBe` (Left 1 :: Either Int Int)
    it "Either applicative with 2 Rights" $
      Right (+ 1) <*> Right 1 `shouldBe` (Right 2 :: Either Int Int)
    it "Either applicative with first Right" $
      Right (+ 1) <*> Left 1 `shouldBe` (Left 1 :: Either Int Int)
    it "Either applicative with second Right" $
      Left 10 <*> Right (1 :: Int) `shouldBe` (Left 10 :: Either Int Int)
    it "Either Monad starting with Right" $
      (Right 10 >>= (\a -> Right (a + 1))) `shouldBe` (Right 11 :: Either Int Int)
    it "Either Monad starting with Left" $
      (Left 10 >>= (\a -> Right (a + 1))) `shouldBe` (Left 10 :: Either Int Int)
    it "Combines two Rights" $
      Right [1, 2, 3] <> Right [4, 5, 6] `shouldBe` (Right [1, 2, 3, 4, 5, 6] :: Either Int [Int])
    it "Keeps first Right" $
      Right [1, 2, 3] <> Left 1 `shouldBe` (Right [1, 2, 3] :: Either Int [Int])
    it "Keeps second Right" $
      Left 1 <> Right [4, 5, 6] `shouldBe` (Right [4, 5, 6] :: Either Int [Int])
    it "Keeps first Left" $
      Left 1 <> Left 2 `shouldBe` (Left 1 :: Either Int [Int])
    it "Folds a Left to the default" $
      foldr (+) 1 (Left 10) `shouldBe` (1 :: Int)
    it "Folds a Right and does addition" $
      foldr (+) 1 (Right 10) `shouldBe` (11 :: Int)
    it "Sequences a list of Rights" $
      sequence [Right 1, Right 2] `shouldBe` (Right [1, 2] :: Either Int [Int])
    it "Sequences a list with Lefts in" $
      sequence [Left 1, Right 1] `shouldBe` (Left 1 :: Either Int [Int])
    it "Bimap over Left" $
      bimap (+ 1) (+ 1) (Left (1 :: Int)) `shouldBe` (Left 2)
    it "Bimap over Right" $
      bimap (+ 1) (+ 1) (Right (1 :: Int)) `shouldBe` (Right 2)
