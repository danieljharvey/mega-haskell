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
{-# LANGUAGE RecordWildCards        #-}
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
import qualified Data.Aeson as JSON
import           Data.ByteString.Lazy
import           Data.Kind            (Type)
import qualified Data.Text            as Text

import           Data.Void            (Void)
import           GHC.Generics
import           GHC.Natural
import           GHC.TypeLits

newtype Name
  = Name { getName :: Text.Text }
  deriving (Show, Eq, Ord, Generic, JSON.FromJSON, JSON.ToJSON)

newtype FirstName
  = FirstName { getFirstName :: Name }
  deriving (Show, Eq, Ord, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Surname
  = Surname { getSurname :: Name }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

data Pet
  = Dog
  | Cat
  | Horse
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

-- V3

data NewUser
  = NewUser
    { firstName :: FirstName
    , surname   :: Surname
    , pet       :: Maybe Pet
    , age       :: Natural
    }
    deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

data OldPet
  = OldDog
  | OldCat
  | NoPet
  deriving (Generic, JSON.FromJSON)



-- V2

data OldUser
  = OldUser
    { oldFirstName :: Name
    , oldSurname   :: Name
    , oldPet       :: OldPet
    , oldAge       :: Int
    }
    deriving (Generic, JSON.FromJSON)

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

-- V1

data Older
  = Older { olderFirstName :: String
          , olderSurname   :: String
          , olderPet       :: String
          }
          deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

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

---
  --
data EvenNewerUser
  = EvenNewerUser
    { newerFirstName :: FirstName
    , newerSurname   :: Surname
    , newerPet       :: Pet
    , newerAge       :: Natural
    }
    deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

updateNewUserToEvenNewerUser :: NewUser -> Maybe EvenNewerUser
updateNewUserToEvenNewerUser (NewUser {..})
  = case pet of
      Just aPet -> Just (EvenNewerUser firstName surname aPet age)
      Nothing   -> Nothing

---------------------------------------------------------

class Versioned (pristine :: Symbol) (num :: Nat) where
  type num `VersionOf` pristine :: Type

class Upgradable (pristine :: Symbol) (num :: Nat) where
  upgrade :: (num - 1) `VersionOf` pristine -> (num `VersionOf` pristine)

class MaybeUpgradable (pristine :: Symbol) (num :: Nat) where
  tryUpgrade :: (num - 1) `VersionOf` pristine -> Maybe (num `VersionOf` pristine)

-- every defined Upgradable gives us a free MaybeUpgradable
instance {-# OVERLAPPABLE #-}
  (Upgradable pristine num)
  => MaybeUpgradable pristine num where
  tryUpgrade = Just . upgrade @pristine @num

--
instance Versioned "User" 0 where
  type 0 `VersionOf` "User" = Older

--
instance Versioned "User" 1 where
  type 1 `VersionOf` "User" = OldUser

instance Upgradable "User" 1 where
  upgrade = updateOlderToOldUser

--
instance Versioned "User" 2 where
  type 2 `VersionOf` "User" = NewUser

instance Upgradable "User" 2 where
  upgrade = updateOldUserToNewUser

---
instance Versioned "User" 3 where
  type 3 `VersionOf` "User" = EvenNewerUser

instance MaybeUpgradable "User" 3 where
  tryUpgrade = updateNewUserToEvenNewerUser

---
class Schema
  (pristine :: Symbol)
  (earliest :: Nat)
  (target :: Nat) where
  decodeVia :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( jobs ~ ReversePath earliest target
  , Migrate jobs target pristine
  , Head jobs ~ target
  , Last jobs ~ earliest
  )
  => Schema pristine earliest target where
    decodeVia = migrate @jobs @target @pristine

---

class WeakSchema
  (pristine :: Symbol)
  (earliest :: Nat)
  (target :: Nat) where
  tryDecodeVia :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( jobs ~ ReversePath earliest target
  , TryMigrate jobs target pristine
  , Head jobs ~ target
  , Last jobs ~ earliest
  )
  => WeakSchema pristine earliest target where
    tryDecodeVia = tryMigrate @jobs @target @pristine

---

class Migrate (versions :: [Nat]) (target :: Nat) (pristine :: Symbol) where
  migrate :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( Migrate (y ': xs) target pristine
  , GenerallyUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => Migrate (this ': y ': xs) target pristine where
  migrate a
    =   decodeAndUpdate @this @target @pristine a
    <|> migrate  @(y ': xs) @target @pristine a

instance
  ( GenerallyUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => Migrate '[this] target pristine where
  migrate a
    = decodeAndUpdate @this @target @pristine a

decodeAndUpdate 
  :: forall this target pristine 
   . GenerallyUpdate this target pristine
  => JSON.FromJSON (this `VersionOf` pristine)
  => ByteString
  -> Maybe (target `VersionOf` pristine)
decodeAndUpdate a
  =   generallyUpdate @this @target @pristine
  <$> JSON.decode @(this `VersionOf` pristine) a

---

class TryMigrate (versions :: [Nat]) (target :: Nat) (pristine :: Symbol) where
  tryMigrate :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( TryMigrate (y ': xs) target pristine
  , MaybeUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => TryMigrate (this ': y ': xs) target pristine where
    tryMigrate a
      =   tryDecodeAndUpdate @this @target @pristine a
      <|> tryMigrate  @(y ': xs) @target @pristine a
 

instance
  ( MaybeUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => TryMigrate '[this] target pristine where
    tryMigrate = tryDecodeAndUpdate @this @target @pristine
  

tryDecodeAndUpdate 
  :: forall this target pristine 
   . MaybeUpdate this target pristine
  => JSON.FromJSON (this `VersionOf` pristine)
  => ByteString
  -> Maybe (target `VersionOf` pristine)
tryDecodeAndUpdate
  =  JSON.decode @(this `VersionOf` pristine)
 >=> maybeUpdate @this @target @pristine

---

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
      , Versioned pristine y
      , Upgradable pristine y
      , GenerallyUpdate_ (y ': xs) pristine
      )
      => GenerallyUpdate_ (x ': y ': xs) pristine where
  generallyUpdate_
    = upgrade @pristine @y
    >>> generallyUpdate_ @(y ': xs) @pristine

---


class MaybeUpdate (earliest :: Nat) (target :: Nat) (pristine :: Symbol) where
  maybeUpdate :: earliest `VersionOf` pristine -> Maybe (target `VersionOf` pristine)

instance
    ( jobs ~ FindPath earliest target
    , MaybeUpdate_ jobs pristine
    , Last jobs ~ target
    , Head jobs ~ earliest
    )
    => MaybeUpdate earliest target pristine where
  maybeUpdate = maybeUpdate_ @jobs @pristine

class MaybeUpdate_ (versions :: [Nat]) (pristine :: Symbol) where
  maybeUpdate_
    :: (Head versions) `VersionOf` pristine
    -> Maybe ((Last versions) `VersionOf` pristine)

instance MaybeUpdate_ '[n] pristine where
  maybeUpdate_ = pure

instance {-# OVERLAPPABLE #-}
      ( x ~ (y - 1)
      , Versioned pristine y
      , MaybeUpgradable pristine y
      , MaybeUpdate_ (y ': xs) pristine
      )
      => MaybeUpdate_ (x ': y ': xs) pristine where
  maybeUpdate_
    = tryUpgrade @pristine @y
      >=> maybeUpdate_ @(y ': xs) @pristine

---

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

type family FindPath (m :: Nat) (n :: Nat) :: [Nat] where
  FindPath m m = '[m]
  FindPath m n = m ': FindPath (m + 1) n
