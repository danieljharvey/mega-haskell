module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)
import Tests.EffectAndAff as EffectAndAff

main :: Effect Unit
main = runTest do
  EffectAndAff.tests
