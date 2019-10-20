module StateSpec where

import Control.Exception (evaluate)
import Control.Monad.State
import State
import Test.Hspec
import Test.QuickCheck

spec = do
  describe "className"
    $ it "Does the bare minimum"
    $ className 1 `shouldBe` "genClass1"
  describe "renderStyleClass"
    $ it "Makes a class name"
    $ renderStyleClass "gen100" `shouldBe` "class=\"gen100\""
  describe "getStyleNumber" $ do
    it "Does nothing with empty style" $
      getStyleNumber [] [] `shouldBe` (Nothing, [])
    it "Adds a style to an empty list and returns 0" $
      getStyleNumber [] [Color "red"] `shouldBe` (Just 0, [[Color "red"]])
    it "Adds a new to a list and returns the index" $
      getStyleNumber [[Color "red"]] [Color "blue"] `shouldBe` (Just 1, [[Color "red"], [Color "blue"]])
    it "Returns an existing value from the list and it's index" $
      getStyleNumber [[Color "red"], [Color "blue"]] [Color "blue"] `shouldBe` (Just 1, [[Color "red"], [Color "blue"]])
  describe "getStyleNumber" $ do
    it "Does nothing with empty style" $
      runState (getStateStyleClass []) [] `shouldBe` (Nothing, [])
    it "Adds a style to an empty list and returns 0" $
      runState (getStateStyleClass [Color "red"]) [] `shouldBe` (Just 0, [[Color "red"]])
    it "Adds a new to a list and returns the index" $
      runState (getStateStyleClass [Color "blue"]) [[Color "red"]] `shouldBe` (Just 1, [[Color "red"], [Color "blue"]])
    it "Returns an existing value from the list and it's index" $
      runState (getStateStyleClass [Color "blue"]) [[Color "red"], [Color "blue"]] `shouldBe` (Just 1, [[Color "red"], [Color "blue"]])
  describe "creates style tag from int" $ do
    it "composes" $
      (renderStyleClass . className) 4 `shouldBe` "class=\"genClass4\""
    it "uses getClassTag with Just" $
      getClassTag (Just 10) `shouldBe` "class=\"genClass10\""
    it "uses getClassTag with Nothing" $
      getClassTag Nothing `shouldBe` ""
  describe "renderCSS" $ do
    it "Ignores empty class" $
      renderCSS 100 [] `shouldBe` ""
    it "Renders one property" $
      renderCSS 100 [Color "red"] `shouldBe` ".genClass100 {\ncolor: red;\n}"
    it "Renders two properties" $
      renderCSS 100 [Color "red", Width 100] `shouldBe` ".genClass100 {\ncolor: red;\nwidth: 100px;\n}"
