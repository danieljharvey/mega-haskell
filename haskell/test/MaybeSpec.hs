module MaybeSpec where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Identity hiding (fail)
import Control.Monad.Zip
import Data.Semigroup
import Maybe
import Test.Hspec
import Prelude hiding (Maybe (..), fail)

type MaybeIdentity = MaybeT Identity

plusOne :: Int -> Int
plusOne = (+ 1)

spec :: Spec
spec =
  describe "Maybe" $ do
    it "Maybe functor with Just" $
      fmap plusOne (Just (1 :: Int)) `shouldBe` Just 2
    it "Maybe functor with Nothing" $
      fmap plusOne Nothing `shouldBe` Nothing
    it "Maybe applicative (pure)" $
      pure (1 :: Int) `shouldBe` Just 1
    it "Maybe applicative (<*>)" $
      Just plusOne <*> Just 1 `shouldBe` Just 2
    it "Maybe applicative <*> with Nothing" $
      Just plusOne <*> Nothing `shouldBe` Nothing
    it "Maybe monad with Just" $
      (Just 1 >>= Just) `shouldBe` Just (1 :: Int)
    it "Maybe monad with Nothing" $
      (Nothing >>= Just) `shouldBe` (Nothing :: Maybe Int)
    it "Maybe Semigroup with Just" $
      Just [1, 2, 3] <> Just [4, 5, 6] `shouldBe` Just ([1, 2, 3, 4, 5, 6] :: [Int])
    it "Maybe Semigroup with Nothing" $
      Nothing <> Just [1, 2, 3] `shouldBe` Just ([1, 2, 3] :: [Int])
    it "Maybe Semigroup with other Nothing" $
      Just [1, 2, 3] <> Nothing `shouldBe` Just ([1, 2, 3] :: [Int])
    it "Maybe Semigroup with all Nothing" $
      Nothing <> Nothing `shouldBe` (Nothing :: Maybe [Int])
    it "Maybe Monoid" $
      mempty `shouldBe` (Nothing :: Maybe [Int])
    it "Foldable Maybe with Nothing" $
      foldr (+) 1 Nothing `shouldBe` (1 :: Int)
    it "Foldable Maybe with Just" $
      foldr (+) 1 (Just 10) `shouldBe` (11 :: Int)
    it "Alternative ending in nothing" $
      (Nothing <|> Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "Alternative first" $
      (Just (1 :: Int) <|> Just (2 :: Int)) `shouldBe` Just 1
    it "Alternative second" $
      (Nothing <|> Just 2) `shouldBe` Just (2 :: Int)
    it "Traverses list Nothing" $
      traverse (\a -> [a, a]) (Nothing :: Maybe Int) `shouldBe` [Nothing]
    it "Traverses list and Just 1" $
      (traverse (\a -> [a, a]) $ Just (10 :: Int)) `shouldBe` [Just 10, Just 10]
    it "Monad fail returns Nothing" $
      (Just "yes" >>= fail) `shouldBe` (Nothing :: Maybe String)
    it "MonadPlus append Nothings" $
      (Nothing `mplus` Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "MonadPlus append first Just" $
      (Just 1 `mplus` Nothing) `shouldBe` Just (1 :: Int)
    it "MonadPlus append second Just" $
      (Nothing `mplus` Just 2) `shouldBe` Just (2 :: Int)
    it "MonadPlus empty" $
      (mzero :: Maybe Int) `shouldBe` Nothing
    it "MonadZip mzipWith" $
      mzip (Just 1) (Just 2) `shouldBe` Just ((1, 2) :: (Int, Int))
    it "MonadZip mzipWith fail" $
      mzip (Just 1) Nothing `shouldBe` (Nothing :: Maybe (Int, Int))
    it "MaybeT Fmaps" $
      runMaybeT (fmap (+ 1) (pure 1 :: MaybeIdentity Int))
        `shouldBe` (Identity (Just 2) :: Identity (Maybe Int))
    it "MaybeT pure" $
      runMaybeT (pure 1 :: MaybeIdentity Int) `shouldBe` Identity (Just 1)
    it "MaybeT <*>" $
      (runMaybeT $ (pure (+ 1) <*> pure (1 :: Int))) `shouldBe` Identity (Just 2)
    it "MaybeT monad" $ do
      p <- pure (1 :: Int)
      p `shouldBe` 1
    it "MaybeT folds" $
      foldr (+) 0 (pure 1 :: MaybeIdentity Int) `shouldBe` 1
    it "MaybeT alternative" $
      runMaybeT (empty <|> pure (1 :: Int)) `shouldBe` Identity (Just 1)
