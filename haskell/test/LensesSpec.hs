module LensesSpec where

import Control.Lens
import Lenses
import Test.Hspec

spec :: Spec
spec = do
  describe "Manually getting data" $ do
    it "gets port" $
      getPort appData `shouldBe` 8080
    it "increments the port" $
      getPort (incrementPort appData) `shouldBe` 8081
    it "sets the port" $
      getPort (setPort 10 appData) `shouldBe` 10
    it "gets 'count' when it's Right" $
      getCountInt appData `shouldBe` Just 100
    it "doesn't get 'count' when it's Right" $
      getCountError appData `shouldBe` Nothing
    it "sets and fetches a new Right" $
      getCountInt (setCountInt 666 appData) `shouldBe` Just 666
    it "sets and fetches a new Left" $
      getCountError (setCountError "Horses" appData) `shouldBe` Just "Horses"
  describe "Prisms" $ do
    it "Gets the string" $
      preview dogNamePrism spruceBruce `shouldBe` Just "Spruce Bruce"
    it "Gets no string" $
      preview dogAgePrism spruceBruce `shouldBe` Nothing
    it "Gets the int" $
      preview dogAgePrism oldDog `shouldBe` Just 100
    it "Gets no int" $
      preview dogNamePrism oldDog `shouldBe` Nothing
    it "Changes name on dogString" $
      preview dogNamePrism (set dogNamePrism "Regular Bruce" spruceBruce) `shouldBe` Just "Regular Bruce"
    it "Can't change name on dogInt" $
      preview dogNamePrism (set dogNamePrism "Bruce" oldDog) `shouldBe` Nothing
    it "Changes age on dogInt" $
      preview dogAgePrism (set dogAgePrism 27 oldDog) `shouldBe` Just 27
    it "Can't change age on dogString" $
      preview dogAgePrism (set dogAgePrism 27 spruceBruce) `shouldBe` Nothing
    it "Get dog age" $
      getDogAge oldDog `shouldBe` Just 100
    it "Get dog name" $
      getDogName spruceBruce `shouldBe` Just "Spruce Bruce"
  describe "Static values" $ do
    it "Dog age" $
      dogAge `shouldBe` Just 100
    it "Not dog age" $
      notDogAge `shouldBe` Nothing
    it "Dog name" $
      dogName `shouldBe` Just "Spruce Bruce"
    it "Not dog name" $
      notDogName `shouldBe` Nothing
    it "New age" $
      newAge `shouldBe` DogAge 27
    it "No new age" $
      noNewAge `shouldBe` DogName "Spruce Bruce"
    it "initialCount" $
      initialCount `shouldBe` Just 100
  describe "The same with sweet, sweet Lens" $ do
    it "gets port with lens" $
      view fullPortLens appData `shouldBe` 8080
    it "increments the port with lens" $
      getPort (over fullPortLens (+ 1) appData) `shouldBe` 8081
    it "sets the port with lens" $
      getPort (set fullPortLens 10 appData) `shouldBe` 10
    it "gets 'count' when it's Right with lens" $
      (preview fullCountInt appData) `shouldBe` Just 100
    it "doesn't get 'count' when it's Right with lens" $
      (preview fullCountError appData) `shouldBe` Nothing
    it "sets and fetches a new Right with lens" $
      getCountInt (set fullCountInt 666 appData) `shouldBe` Just 666
    it "can't set a new Left with the prism" $
      getCountInt (set fullCountError "Horses" appData) `shouldBe` Just 100
    it "must change the sum further up the tree with lens" $
      getCountError (set countLens (Left "Horses") appData) `shouldBe` Just "Horses"
