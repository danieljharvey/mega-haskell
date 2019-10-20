module MonadSpec where

import Control.Monad.Writer
import Monad
import Test.Hspec
import Prelude hiding (Monad)

-- spec :: IO ()
spec =
  describe "Monad" $ do
    describe "Id" $ do
      it "PlainVal is 7" $
        plainVal `shouldBe` 7
      it "Functor" $
        doubled `shouldBe` Id 14
      it "Applicative pure" $
        idValue `shouldBe` Id "Hello!"
      it "Applicative apply" $
        getLength `shouldBe` Id 4
      it "Monad" $
        doubleAndWrap 1 `shouldBe` Id 2
      it "Monadic do" $
        doubleAFewTimes 10 `shouldBe` Id 160
    describe "Maybe" $ do
      it "head3 with empty" $
        head3 ([] :: [[[Int]]]) `shouldBe` Nothing
      it "head3 with stuff" $
        head3 [[[1, 2, 3]]] `shouldBe` Just 1
    describe "Either" $ do
      it "Is empty" $
        validate "" `shouldBe` Left IsEmpty
      it "Contains horse" $
        validate "bah horse" `shouldBe` Left ContainsHorse
      it "Too long" $
        validate "really long string" `shouldBe` Left TooLong
      it "Just right" $
        validate "Hello" `shouldBe` Right "Hello"
    describe "List" $ do
      it "Wee thing" $
        moreList 1 `shouldBe` [0, 1, 2]
      it "Makes one big list" $
        lotsMoreList 1 `shouldBe` [-1, 0, 1, 0, 1, 2, 1, 2, 3]
    describe "Reader"
      $ it "Runs withConfig"
      $ withConfig `shouldBe` "The ip address is 127.0.0.1, the name is localhost"
    describe "Writer"
      $ it "Runs maths"
      $ runWriter (maths 10) `shouldBe` (22, "Add one times two ")
