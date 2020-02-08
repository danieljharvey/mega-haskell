{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module ContractTests where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import Data.Maybe (catMaybes)
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen (generate, oneof)
import Prelude

data Horse
  = BigHorse
  | SmallHorse
  deriving (Generic, FromJSON)

instance Arbitrary Horse where
  arbitrary =
    oneof
      [ pure BigHorse,
        pure SmallHorse
      ]

data APIRequest
  = APIRequest
      { name :: String,
        age :: Int,
        horseSize :: Horse
      }
  deriving (Generic, FromJSON)

instance Arbitrary APIRequest where
  arbitrary = genericArbitrary

-- creating our responses

data APIResponse
  = APIResponse
      { weight :: Int,
        goodHorse :: Bool
      }
  deriving (Generic, ToJSON)

instance Arbitrary APIResponse where
  arbitrary = genericArbitrary

-- this will generate 100 APIResponses
getResponses :: (Arbitrary a) => IO [a]
getResponses = generate $ vector 100

-- create our responses, turn them to JSON, add numbers for file naming
listToJSON :: (ToJSON a) => [a] -> [(Int, BS.ByteString)]
listToJSON = (indexList . jsonifyList)

-- combine these functions
-- because we never use the `a` directly, we need to use TypeApplications to
-- tell the function what it is as it cannot infer it from the use
responsesToJSON :: IO [(Int, BS.ByteString)]
responsesToJSON = listToJSON <$> getResponses @APIResponse

-- this will turn a pile of responses into a pile of JSON responses
jsonifyList :: (ToJSON a) => [a] -> [BS.ByteString]
jsonifyList = fmap encode

-- save a json file using the path, number and JSON bytestring
saveFile :: String -> (Int, BS.ByteString) -> IO ()
saveFile path (index, json) =
  BS.writeFile (createPath path index) json
  where
    createPath path index =
      "./" <> path <> "/" <> (show index) <> ".json"

-- turn a list of anything into a list of tuples with an index in the first
-- position
-- ['A', 'B'] -> [(1, 'A'), (2, 'B')]
indexList :: [a] -> [(Int, a)]
indexList as =
  List.zip [1 ..] as

{- reading our requests and checking they are OK -}

-- read the file from the path and index provided, and try to decode it
testFile :: FromJSON a => String -> Int -> IO (Maybe a)
testFile path i = do
  let buildPath i path =
        path <> "/" <> (show i) <> ".json"
  str <- BS.readFile (buildPath i path)
  case eitherDecode str of
    Left e -> putStrLn (show e) >>= \_ -> pure Nothing
    Right b -> pure (Just b)

-- try to decode 1.json, 2.json from the given path
-- will return only the successful results
readRequests :: FromJSON a => String -> IO [a]
readRequests from = do
  let nums = [1 .. 100]
  maybeFound <- mapM (testFile from) nums
  pure $ catMaybes maybeFound

-- this newtype can derive Arbitrary via Generic, so we use Deriving Via to
-- steal it's powers!

newtype GenericArb a
  = GenericArb {getGenericArb :: a}
  deriving (Generic)

instance (Generic a, Arbitrary a) => Arbitrary (GenericArb a) where
  arbitrary = genericArbitrary

--

data APIRequest2
  = APIRequest2
      { name2 :: String,
        age2 :: Int,
        horseSize2 :: Horse
      }
  deriving (Generic, FromJSON)
  deriving (Arbitrary) via (GenericArb APIRequest2)

data APIResponse2
  = APIResponse2
      { weight2 :: Int,
        goodHorse2 :: Bool
      }
  deriving (Generic, ToJSON)
  deriving (Arbitrary) via (GenericArb APIResponse2)
