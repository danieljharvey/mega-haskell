module AlternativeSpec where

import           Alternative
import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Alternative" $ do
    describe "Applicative" $ do
      it "Doesn't break with second Nope" $
        Yeah (+1) <*> Nope `shouldBe` (Nope :: Perhaps Int)
      it "Doesn't break with first Nope" $
        Nope <*> Yeah 1 `shouldBe` (Nope :: Perhaps Int)
      it "Works with two Yeahs" $
        Yeah (+1) <*> Yeah 1 `shouldBe` Yeah 2
    describe "getPreferred" $ do
      it "returns nope" $
        nah `shouldBe` Nope
      it "returns the second item" $
        found `shouldBe` Yeah 2
      it "returns the first item" $
        fallback `shouldBe` Yeah 1
    describe "Naive implemention" $ do
      it "empty case" $
        naiveImplementation [] `shouldBe` (Nope :: Perhaps Int)
      it "finds first item" $
        naiveImplementation [1] `shouldBe` Yeah 1
      it "finds second item" $
        naiveImplementation [1,2] `shouldBe` Yeah 2
    it "finds the gallery route" $
      matchRouteDefault "http://internet.com/gallery" `shouldBe` Gallery
    it "finds the contact route" $
      matchRouteDefault "http://internet.com/contact" `shouldBe` Contact
    it "finds the help route" $
      matchRouteDefault "http://internet.com/help" `shouldBe` Help
    it "finds nothing and uses the default route" $
      matchRouteDefault "http://internet.com/" `shouldBe` Index
