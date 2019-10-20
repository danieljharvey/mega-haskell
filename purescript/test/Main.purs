module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Tests.EffectAndAff as EffectAndAff
import Tests.Refined as Refined
import Tests.Variant as Variant
import Tests.FoldEvents as FoldEvents

main :: Effect Unit
main =
  runTest do
    EffectAndAff.tests
    Refined.tests
    Variant.tests
    FoldEvents.tests
