{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SchemaSpec where

import Data.Aeson
import Data.Maybe
import Schema
import Test.Hspec

-- type equivalence tests

data (x :: k) :~: (y :: k) where
  Refl :: x :~: x

testR :: ReversePath 5 10 :~: '[10, 9, 8, 7, 6, 5]
testR = Refl

testR2 :: ReversePath 0 2 :~: '[2, 1, 0]
testR2 = Refl

test0 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test0 = Refl

test1 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test1 = Refl

test2 :: FindPath 2 2 :~: '[2]
test2 = Refl

spec :: Spec
spec =
  describe "Schema" $ do
    describe "Decoding" $ do
      it "Decodes and converts Older to NewUser" $ do
        let jsonSample = encode (Older "a" "b" "c")
        let tryDecoding = decodeVia @"User" @0 @2 jsonSample
        isJust tryDecoding `shouldBe` True
      it "Decodes and converts Older to Older" $ do
        let tryDecoding2 = decodeVia @"User" @0 @0 (encode (Older "bo" "f" "f"))
        isJust tryDecoding2 `shouldBe` True
      it "Can decode any Schema with WeakSchema" $ do
        let jsonSample = encode (Older "don't" "do" "drugs")
        let tryMaybeDecode = tryDecodeVia @"User" @0 @2 jsonSample
        isJust tryMaybeDecode `shouldBe` True
      it "Fails to convert a WeakSchema where the data is invalid" $ do
        let jsonSample = encode (Older "ham" "man" "wham")
        isJust (tryDecodeVia @"User" @0 @3 jsonSample) `shouldBe` False
      it "Succeeds in converting a WeakSchema" $ do
        let jsonSample = encode (Older "Me" "Yes" "dog")
        isJust (tryDecodeVia @"User" @0 @3 jsonSample) `shouldBe` True
    describe "Converting" $ do
      it "Updates an old object to new" $ do
        let older = Older "What" "Sure" "great"
        let newUser = generallyUpdate @0 @2 @"User" older
        firstName newUser `shouldBe` FirstName (Name "What")
{-
    describe "Json decoding" $ do
      it "Creates a FromJSON instance" $ do
        let jsonSample = encode (Older "Yeah" "Sure" "Why?")
        isJust (decode @UserType jsonSample) `shouldBe` True
-}
