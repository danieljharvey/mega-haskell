module ReaderSpec where

import           Reader
import           Test.Hspec

-- spec :: IO ()
spec =
  describe "Reader" $ do
    it "Runs the most basic Reader" $
      basic `shouldBe` "Hello, Dog"
    it "Functor" $
      functor `shouldBe` "Hello, Dog!!!!"
    it "Applicative" $
      applicative `shouldBe` 10
    it "Monad" $
      runMonad `shouldBe` "Log: Hello, Log/nHello, Log"
