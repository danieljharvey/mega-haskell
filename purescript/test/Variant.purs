module Tests.Variant (tests) where

import Prelude (Unit, discard)
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

import Variant

tests :: Free TestF Unit
tests =
  suite "Variant" do
    test "TryLogin" do
      Assert.equal false tryLogin.loggingIn 
      Assert.equal true tryLogin.loggedIn
      Assert.equal 1 tryLogin.value 
