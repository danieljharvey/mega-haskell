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

module Schema where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy
import qualified Data.Text            as Text
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








-- Versioned captures this idea of bringing data up to date

class WithNumber a (num :: Nat)

instance WithNumber Older 1
instance WithNumber OldUser 2
instance WithNumber NewUser 3

class (FromJSON a, FromJSON b, WithNumber a (num - 1), WithNumber b num)
  => Versioned a b num | b -> num where
    update :: a -> Maybe b

instance ( FromJSON a
         , FromJSON b
         , FromJSON c
         , WithNumber a (num - 1)
         , WithNumber c num
         , Versioned a b (num - 1)
         , Versioned b c num
         ) => Versioned a c num where
  update = update @a @b >=> update @b @c

instance Versioned OldUser NewUser 3 where
  update = updateOldUserToNewUser

instance Versioned Older OldUser 2 where
  update = updateOlderToOldUser

  {-
-- can we generate this recursively?
instance Versioned Older NewUser where
   convert = convert @Older >=> convert @OldUser
-}

  {-
decodeFor :: forall a b n m. (Versioned a b n, WithNumber b m, WithNumber a n, FromJSON a) => ByteString -> Maybe b
decodeFor = decode @a >=> update @a @b
-}

  {-
decodeAllNew :: ByteString -> Maybe NewUser
decodeAllNew bs
  =   decode @NewUser bs
--  <|> decodeFor @OldUser bs
  <|> decode @OldUser bs >>= update
-}
