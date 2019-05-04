module Test.Variant where

import Prelude (Unit, discard, negate, (<$>), (<<<), (==))
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Variant
import Data.Typelevel.Num.Reps (D1, D9)
import Data.Typelevel.Undefined (undefined)

import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)

import Data.Argonaut.Core (fromNumber, fromString, toNumber)

tests :: Free TestF Unit
tests = do
  suite "Variant" do
    test "Module exists" do
      Assert.equal 1 
 
