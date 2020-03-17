module Validation where

import Data.List
import Data.Validation

data Failures = TooLong | TooShort | ContainsEgg deriving (Show)

checkTooLong :: String -> Validation [Failures] String
checkTooLong =
  validate
    [TooLong]
    ( \a ->
        if length a < 10
          then Just a
          else Nothing
    )

lengthResult :: Validation [Failures] String
lengthResult = checkTooLong "dog"

containsEgg :: String -> Validation [Failures] String
containsEgg =
  validate
    [ContainsEgg]
    ( \a ->
        if not ("egg" `isInfixOf` a)
          then Just a
          else Nothing
    )

doesItContainEgg :: Validation [Failures] String
doesItContainEgg = containsEgg "eggeggeggegg"

bothTests :: String -> Validation [Failures] String
bothTests a = containsEgg a <> checkTooLong a

twofails :: Validation [Failures] String
twofails = checkTooLong "dogsdsdfsdfsdfsdf" <> containsEgg "egg"
