{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module ContractTests where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Proxy
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen (generate, oneof)
import Prelude

{- SOURCE DATATYPES -}

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

data APIResponse
  = APIResponse
      { weight :: Int,
        goodHorse :: Bool
      }
  deriving (Generic, ToJSON)

instance Arbitrary APIResponse where
  arbitrary = genericArbitrary

{- GENERATING SAMPLE RESPONSES -}

-- this will generate 100 instances of any given arbitrary value
getResponses :: (Arbitrary a) => Proxy a -> IO [a]
getResponses _ = generate $ vector 100

-- this will turn a pile of responses into a pile of JSON responses
jsonifyList :: (ToJSON a) => [a] -> [BS.ByteString]
jsonifyList = fmap encode

-- create our responses, turn them to JSON, add numbers for file naming
listToJSON :: (ToJSON a) => [a] -> [(Int, BS.ByteString)]
listToJSON = (indexList . jsonifyList)

-- combine these functions
-- because we never use the `a` directly, we need to use TypeApplications to
-- tell the function what it is as it cannot infer it from the use
responsesToJSON ::
  (ToJSON a, Arbitrary a) =>
  Proxy a ->
  IO [(Int, BS.ByteString)]
responsesToJSON arbType = listToJSON <$> (getResponses arbType)

-- save a json file using the path, number and JSON bytestring
saveFile ::
  String ->
  (Int, BS.ByteString) ->
  IO ()
saveFile path (index, json) =
  BS.writeFile (createPath path index) json

-- generate 100 APIResponse values and save them in the srcPath folder
contractWrite ::
  (ToJSON a, Arbitrary a) =>
  Proxy a ->
  String ->
  IO ()
contractWrite arbType srcPath = do
  responses <- responsesToJSON arbType
  mapM_ (saveFile srcPath) responses

-- example of using the Proxy to pass our APIResponse type to contractWrite
contractWriteAPIResponses :: String -> IO ()
contractWriteAPIResponses srcPath =
  contractWrite (Proxy :: Proxy APIResponse) srcPath

{- HELPERS -}

-- turn a list of anything into a list of tuples with an index in the first
-- position
-- ['A', 'B'] -> [(1, 'A'), (2, 'B')]
indexList :: [a] -> [(Int, a)]
indexList as =
  List.zip [1 ..] as

-- take a root path and a file number and return a file path
createPath :: String -> Int -> String
createPath path index =
  "./" <> path <> "/" <> (show index) <> ".json"

{- CHECKING SAMPLE REQUESTS -}

-- read the file from the path and index provided, and try to decode it
testFile :: FromJSON a => Proxy a -> String -> Int -> IO (Maybe a)
testFile _ path i = do
  str <- BS.readFile (createPath path i)
  case eitherDecode str of
    Left e -> putStrLn (show e) >>= \_ -> pure Nothing
    Right b -> pure (Just b)

-- try to decode 1.json, 2.json from the given path
-- will return only the successful results
contractRead :: FromJSON a => Proxy a -> String -> IO Int
contractRead arbType srcPath = do
  maybeFound <- mapM (testFile arbType srcPath) [1 .. 100]
  pure $ length $ catMaybes maybeFound

-- example using Proxy to pass APIRequest type to contractRead
contractReadAPIRequest :: String -> IO Int
contractReadAPIRequest srcPath =
  contractRead (Proxy :: Proxy APIRequest) srcPath

{- An alternative way to generate arbitrary from generic -}

-- this newtype can derive Arbitrary via Generic, so we use Deriving Via to
-- steal it's powers!

newtype GenericArb a
  = GenericArb {getGenericArb :: a}
  deriving (Generic)

instance (Generic a, Arbitrary a) => Arbitrary (GenericArb a) where
  arbitrary = genericArbitrary

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
