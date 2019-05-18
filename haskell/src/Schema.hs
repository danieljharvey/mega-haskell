{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
module Schema where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Kind            (Type)
import qualified Data.Text            as Text

import           Data.Void            (Void)
import           GHC.Generics
import           GHC.Natural
import           GHC.TypeLits

newtype Name
  = Name { getName :: Text.Text }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype FirstName
  = FirstName { getFirstName :: Name }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype Surname
  = Surname { getSurname :: Name }
  deriving (Show, Generic, FromJSON, ToJSON)

data Pet
  = Dog
  | Cat
  | Horse
  deriving (Show, Generic, FromJSON, ToJSON)

-- V3

data NewUser
  = NewUser
    { firstName :: FirstName
    , surname   :: Surname
    , pet       :: Maybe Pet
    , age       :: Natural
    }
    deriving (Show, Generic, FromJSON, ToJSON)

decodeUser :: ByteString -> Maybe NewUser
decodeUser = decode

data OldPet
  = OldDog
  | OldCat
  | NoPet
  deriving (Generic, FromJSON)



-- V2

data OldUser
  = OldUser
    { oldFirstName :: Name
    , oldSurname   :: Name
    , oldPet       :: OldPet
    , oldAge       :: Int
    }
    deriving (Generic, FromJSON)

decodeOldUser :: ByteString -> Maybe OldUser
decodeOldUser = decode

updateOldUserToNewUser :: OldUser -> NewUser
updateOldUserToNewUser old
  = NewUser
      { firstName = FirstName (oldFirstName old)
      , surname   = Surname (oldSurname old)
      , pet       = convertPet (oldPet old)
      , age       = intToNatural (oldAge old)
      }
  where
    convertPet pet
      = case pet of
          OldDog -> Just Dog
          OldCat -> Just Cat
          _      -> Nothing

  {-
-- decode and convert to new user type
decodeOldUserToNewUser :: ByteString -> Maybe NewUser
decodeOldUserToNewUser
  = decode >=> updateOldUserToNewUser
-}



-- V1

data Older
  = Older { olderFirstName :: String
          , olderSurname   :: String
          , olderPet       :: String
          }
          deriving (Show, Generic, FromJSON, ToJSON)

decodeOlder :: ByteString -> Maybe Older
decodeOlder = decode

updateOlderToOldUser :: Older -> OldUser
updateOlderToOldUser older
  = OldUser { oldFirstName = Name (Text.pack (olderFirstName older))
            , oldSurname   = Name (Text.pack (olderSurname older))
            , oldPet       = readPet (olderPet older)
            , oldAge       = 18
            }
  where
    readPet s
      | s == "dog" = OldDog
      | s == "cat" = OldCat
      | otherwise  = NoPet

sampleOlder :: Older
sampleOlder = Older "john" "snoww" "peeboo"

  {-
decodeOlderToNewUser :: ByteString -> Maybe NewUser
decodeOlderToNewUser
  = decode >=> updateOlderToOldUser
           >=> updateOldUserToNewUser
-}
  {-
-- try decoding all versions of this mad bullshit
decodeAll :: ByteString -> Maybe NewUser
decodeAll bs
    = decodeUser bs
  <|> decodeOldUserToNewUser bs
  <|> decodeOlderToNewUser bs
-}

-----------------------------------------------------------

data User = User { tomName :: String, tomAge :: Int }

class TomVersioned (pristine :: Symbol) (num :: Nat) where
  type num `VersionOf` pristine :: Type

  upgrade :: (num - 1) `VersionOf` pristine -> (num `VersionOf` pristine)

instance TomVersioned "User" 0 where
  type 0 `VersionOf` "User" = Older

  upgrade = undefined

instance TomVersioned "User" 1 where
  type 1 `VersionOf` "User" = OldUser

  upgrade = updateOlderToOldUser

instance TomVersioned "User" 2 where
  type 2 `VersionOf` "User" = NewUser

  upgrade = updateOldUserToNewUser

-- usage , 5 10
class TryDecoding (earliest :: Nat) (target :: Nat) (pristine :: Symbol) where
  tryDecode :: ByteString -> Maybe (target `VersionOf` pristine)

{- the simple case where there is only one version -}
instance (FromJSON (VersionOf i pristine))
  => TryDecoding i i pristine where
    tryDecode = decode

instance
  ( jobs ~ ReversePath earliest target
  , TryDecoding_ jobs target pristine
  , Head jobs ~ target
  , Last jobs ~ earliest
  )
  => TryDecoding earliest target pristine where
    tryDecode
      = tryDecode_ @jobs @target @pristine

class TryDecoding_ (versions :: [Nat]) (target :: Nat) (pristine :: Symbol) where
  tryDecode_ :: ByteString -> Maybe (target `VersionOf` pristine)

instance (FromJSON (n `VersionOf` pristine)) => TryDecoding_ '[n] n pristine where
  tryDecode_ = decode

instance {-# OVERLAPPABLE #-}
  ( TomVersioned pristine y
  , TomVersioned pristine this
  , TomVersioned pristine target
  , TryDecoding_ (y ': xs) target pristine
  , GenerallyUpdate this target pristine
  , FromJSON (this `VersionOf` pristine)
  ) => TryDecoding_ (this ': y ': xs) target pristine where
  tryDecode_ a
    = decodeAndUpdate a
    <|> tryDecode_  @(y ': xs) @target @pristine a
    where
      decodeAndUpdate a
        = generallyUpdate @this @target @pristine
        <$> decode @(this `VersionOf` pristine) a

instance
  ( TomVersioned pristine this
  , TomVersioned pristine target
  , GenerallyUpdate this target pristine
  , FromJSON (this `VersionOf` pristine)
  ) => TryDecoding_ '[this] target pristine where
  tryDecode_ a
    = generallyUpdate @this @target @pristine
        <$> decode @(this `VersionOf` pristine) a

class GenerallyUpdate (earliest :: Nat) (target :: Nat) (pristine :: Symbol) where
  generallyUpdate :: earliest `VersionOf` pristine -> (target `VersionOf` pristine)

instance
    ( jobs ~ FindPath earliest target
    , GenerallyUpdate_ jobs pristine
    , Last jobs ~ target
    , Head jobs ~ earliest
    )
    => GenerallyUpdate earliest target pristine where
  generallyUpdate = generallyUpdate_ @jobs @pristine

class GenerallyUpdate_ (versions :: [Nat]) (pristine :: Symbol) where
  generallyUpdate_
    :: (Head versions) `VersionOf` pristine
    -> ((Last versions) `VersionOf` pristine)

instance GenerallyUpdate_ '[n] pristine where
  generallyUpdate_ = id

instance {-# OVERLAPPABLE #-}
      ( x ~ (y - 1)
      , TomVersioned pristine y
      , GenerallyUpdate_ (y ': xs) pristine
      )
      => GenerallyUpdate_ (x ': y ': xs) pristine where
  generallyUpdate_ = upgrade @pristine @y >>> generallyUpdate_ @(y ': xs) @pristine

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Head (xs :: [k]) :: k where
  Head (x ': _) = x

type family Last (xs :: [k]) :: k where
  Last '[x] = x
  Last (_ ': xs) = Last xs

-- |Helper type family for 'Reverse'.
type family ReverseAcc xs acc where
    ReverseAcc '[] acc = acc
    ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

-- |Reverse a type-level list.
type family Reverse xs where
    Reverse xs = ReverseAcc xs '[]

type family ReversePath (m :: Nat) (n :: Nat) :: [Nat] where
  ReversePath m n = Reverse (FindPath m n)

testR :: ReversePath 5 10 :~: '[10, 9, 8, 7, 6, 5]
testR = Refl

testR2 :: ReversePath 0 2 :~: '[2, 1, 0]
testR2 = Refl

data (x :: k) :~: (y :: k) where
  Refl :: x :~: x

type family FindPath (m :: Nat) (n :: Nat) :: [Nat] where
  FindPath m m = '[m]
  FindPath m n = m ': FindPath (m + 1) n

test0 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test0 = Refl

test1 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test1 = Refl

test2 :: FindPath 2 2 :~: '[2]
test2 = Refl

tryDecoding :: Maybe NewUser
tryDecoding = tryDecode @0 @2 @"User" json
  where
    json :: ByteString
    json = encode (Older "b" "b" "c")

updateAll :: Older -> NewUser
updateAll = generallyUpdate @0 @2 @"User"
