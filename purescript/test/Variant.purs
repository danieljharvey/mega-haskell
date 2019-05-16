module Tests.Variant (tests) where

import Prelude (Unit)
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert


tests :: Free TestF Unit
tests =
  suite "Variant" do
    test "createStore" do
      Assert.equal false false
