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
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
module Schema where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Kind            (Type)
import qualified Data.Text            as Text
import           Data.Type.Bool
import           Data.Void            (Void)
import           GHC.Generics
import           GHC.Natural
import           GHC.TypeLits

newtype Name
  = Name { getName :: Text.Text }
  deriving (Generic, FromJSON)

newtype FirstName
  = FirstName { getFirstName :: Name }
  deriving (Generic, FromJSON)

newtype Surname
  = Surname { getSurname :: Name }
  deriving (Generic, FromJSON)

data Pet
  = Dog
  | Cat
  | Horse
  deriving (Generic, FromJSON)

-- V3

data NewUser
  = NewUser
    { firstName :: FirstName
    , surname   :: Surname
    , pet       :: Maybe Pet
    , age       :: Natural
    }
    deriving (Generic, FromJSON)

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

updateOldUserToNewUser :: OldUser -> Maybe NewUser
updateOldUserToNewUser old
  = if (oldAge old) > 0
    then Just $ createUserFromAge (oldAge old)
    else Nothing
  where
    createUserFromAge i
      = NewUser
          { firstName = FirstName (oldFirstName old)
          , surname   = Surname (oldSurname old)
          , pet       = convertPet (oldPet old)
          , age       = intToNatural i
          }

    convertPet pet
      = case pet of
          OldDog -> Just Dog
          OldCat -> Just Cat
          _      -> Nothing

-- decode and convert to new user type
decodeOldUserToNewUser :: ByteString -> Maybe NewUser
decodeOldUserToNewUser
  = decode >=> updateOldUserToNewUser

-- V1

data Older
  = Older { olderFirstName :: String
          , olderSurname   :: String
          , olderPet       :: String
          }
          deriving (Generic, FromJSON)

decodeOlder :: ByteString -> Maybe Older
decodeOlder = decode

updateOlderToOldUser :: Older -> Maybe OldUser
updateOlderToOldUser older
  = pure $
    OldUser { oldFirstName = Name (Text.pack (olderFirstName older))
            , oldSurname   = Name (Text.pack (olderSurname older))
            , oldPet       = readPet (olderPet older)
            , oldAge       = 18
            }
  where
    readPet s
      | s == "dog" = OldDog
      | s == "cat" = OldCat
      | otherwise  = NoPet

decodeOlderToNewUser :: ByteString -> Maybe NewUser
decodeOlderToNewUser
  = decode >=> updateOlderToOldUser
           >=> updateOldUserToNewUser

-- try decoding all versions of this mad bullshit
decodeAll :: ByteString -> Maybe NewUser
decodeAll bs
    = decodeUser bs
  <|> decodeOldUserToNewUser bs
  <|> decodeOlderToNewUser bs


-----------------------------------------------------------

data User = User { tomName :: String, tomAge :: Int }

class TomVersioned (pristine :: Symbol) (num :: Nat) where
  type num `VersionOf` pristine :: Type

  upgrade :: (num - 1) `VersionOf` pristine -> Maybe (num `VersionOf` pristine)

instance TomVersioned "User" 0 where
  type 0 `VersionOf` "User" = Older

  upgrade _ = Nothing

instance TomVersioned "User" 1 where
  type 1 `VersionOf` "User" = OldUser

  upgrade = updateOlderToOldUser

instance TomVersioned "User" 2 where
  type 2 `VersionOf` "User" = NewUser

  upgrade = updateOldUserToNewUser


class GenerallyUpdate (m :: Nat) (n :: Nat) (pristine :: Symbol) where
  generallyUpdate :: m `VersionOf` pristine -> Maybe (n `VersionOf` pristine)

instance
    ( jobs ~ FindPath m n
    , GenerallyUpdate_ jobs pristine
    , Last jobs ~ n
    , Head jobs ~ m
    )
    => GenerallyUpdate m n pristine where
  generallyUpdate = generallyUpdate_ @jobs @pristine

class GenerallyUpdate_ (versions :: [Nat]) (pristine :: Symbol) where
  generallyUpdate_ :: (Head versions) `VersionOf` pristine -> Maybe ((Last versions) `VersionOf` pristine)

instance GenerallyUpdate_ '[n] pristine where
  generallyUpdate_ = pure

instance {-# OVERLAPPABLE #-}
      ( x ~ (y - 1)
      , TomVersioned pristine y
      , GenerallyUpdate_ (y ': xs) pristine
      )
      => GenerallyUpdate_ (x ': y ': xs) pristine where
  generallyUpdate_ = upgrade @pristine @y >=> generallyUpdate_ @(y ': xs) @pristine

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Head (xs :: [k]) :: k where
  Head (x ': _) = x

type family Last (xs :: [k]) :: k where
  Last '[x] = x
  Last (_ ': xs) = Last xs

type family FindPath (m :: Nat) (n :: Nat) :: [Nat] where
  FindPath m m = '[m]
  FindPath m n = m ': FindPath (m + 1) n

data (x :: k) :~: (y :: k) where
  Refl :: x :~: x

test0 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test0 = Refl

test1 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test1 = Refl

test2 :: FindPath 2 2 :~: '[2]
test2 = Refl


f :: Older -> Maybe NewUser
f = generallyUpdate @0 @2 @"User"
