module WriteSpec where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck
import           Write

spec = do
  describe "Style" $ do
    it "Renders a Color" $ do
      (renderStyle $ Color "#FF00FF") `shouldBe` "color: #FF00FF"
    it "Renders a BackgroundColor" $ do
      (renderStyle $ BackgroundColor "#FF00FF") `shouldBe` "background-color: #FF00FF"
    it "Renders a Width" $ do
      (renderStyle $ Width 100) `shouldBe` "width: 100px"
    it "Renders a Height" $ do
      (renderStyle $ Height 100) `shouldBe` "height: 100px"
    it "Renders a whole style unit" $ do
      renderStyles [Color "red", Width 200] `shouldBe` "style=\"color: red, width: 200px\""
  describe "StyleTree" $ do
    it "Renders a whole StyleTree" $ do
      (showStyleTree $ Div [BackgroundColor "#ffeedd"] []) `shouldBe` "<div style=\"background-color: #ffeedd\"></div>"
