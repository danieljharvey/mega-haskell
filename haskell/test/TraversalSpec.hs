module TraversalSpec where

import Control.Exception (evaluate)
import Data.Validation
import Test.Hspec
import Traversal

-- spec :: IO ()
spec = do
  describe "traversal" $ do
    it "foldMaps tree" $
      sampleTreeTotal `shouldBe` 12
    it "traverses maybeTree" $
      justTree `shouldBe` Just (Branch (Branch (Leaf 2) (Leaf 3)) (Branch (Leaf 5) (Leaf 2)))
    it "traverses anotherMaybeTree" $
      nothingTree `shouldBe` Nothing
    it "invertedListTree" $
      invertedListTree `shouldBe` [Branch (Leaf 1) (Leaf 3), Branch (Leaf 1) (Leaf 4), Branch (Leaf 2) (Leaf 3), Branch (Leaf 2) (Leaf 4)]
    it "reversedListTree" $
      reversedListTree `shouldBe` [Branch (Leaf 2) (Leaf 4), Branch (Leaf 2) (Leaf 3), Branch (Leaf 1) (Leaf 4), Branch (Leaf 1) (Leaf 3)]
    it "rightTree" $
      rightTree `shouldBe` Right (Branch (Leaf 100) (Leaf 200))
    it "failsTree" $
      failsTree `shouldBe` Left "2"
    it "failsTree2" $
      failsTree2 `shouldBe` Left "1"
    it "collects the fails" $
      collectFails `shouldBe` Failure ["2", "3"]
