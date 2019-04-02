module Validation where

import           Data.List
import           Data.Semigroup
import           Data.Validation

data Failures = TooLong | TooShort | ContainsEgg deriving (Show)

checkTooLong :: String -> Validation [Failures] String
checkTooLong = validate [TooLong] (\a -> length a < 10)

lengthResult :: Validation [Failures] String
lengthResult = checkTooLong "dog"

containsEgg :: String -> Validation [Failures] String
containsEgg = validate [ContainsEgg] (\a -> not $ "egg" `isInfixOf` a)

doesItContainEgg = containsEgg "eggeggeggegg"

bothTests a = containsEgg a <> checkTooLong a

twofails = checkTooLong "dogsdsdfsdfsdfsdf" <> containsEgg "egg"
