module Tests.EffectAndAff (tests) where

import Prelude
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

import EffectAndAff (affTimer)

tests :: Free TestF Unit
tests =
  suite "EffectAndAff" do
    test "Aff" do
      (affTimer 100) >>= (\answer -> Assert.equal answer 100)
