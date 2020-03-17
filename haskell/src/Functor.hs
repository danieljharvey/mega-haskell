module Functor where

import Data.Char

data Perhaps a = Yeah a | Nah deriving (Eq, Show)

instance Functor Perhaps where
  fmap _ Nah = Nah
  fmap f (Yeah a) = Yeah (f a)

nope :: Perhaps String
nope = Nah

john :: Perhaps String
john = Yeah "John"

questionAdd :: Perhaps String -> Perhaps String
questionAdd Nah = Nah
questionAdd (Yeah name) = Yeah (name ++ "???")

-- questionAdd Nah  = Nah
-- questionAdd john = Yeah "John???"

exclaimAdd :: Perhaps String -> Perhaps String
exclaimAdd Nah = Nah
exclaimAdd (Yeah name) = Yeah (name ++ "!")

-- exclaimAdd Nah  = Nah
-- exclaimAdd john = Yeah "John!"

exclaim :: String -> String
exclaim str = str ++ "!!!!!!!!!!!!"

-- exclaim "Horse" == "Horse!!!!!!!!!!!!"

capitalise :: String -> String
capitalise str = toUpper <$> str

-- capitalise "Horse" == "HORSE"

veryJohn :: Perhaps String
veryJohn = fmap exclaim john

-- veryJohn == Yeah "John!!!!!!!!!!!!"

stillNope :: Perhaps String
stillNope = fmap exclaim nope

-- stillNope = Nah

returnA :: a -> a
returnA a = a

-- this is also called id in the Prelude

identityLaw :: Perhaps String -> Bool
identityLaw j = j == fmap id j

-- identityLaw = True

shouting :: Perhaps String -> Perhaps String
shouting p = fmap (capitalise . exclaim) p

-- shouting (Yeah "Bruce") == Yeah "BRUCE!!!!!!!!!!!!"

shouting2 :: Perhaps String -> Perhaps String
shouting2 p = fmap capitalise (fmap exclaim p)

-- shouting2 (Yeah "Bruce") == Yeah "BRUCE!!!!!!!!!!!!"

compositionLaw :: Perhaps String -> Bool
compositionLaw j =
  fmap (capitalise . exclaim) j
    == fmap capitalise (fmap exclaim j)

-- either way of doing this ends up the same

data Poohoops a = Yerp a | Nerp deriving (Eq, Show)

instance Functor Poohoops where
  fmap _ Nerp = Nerp
  fmap _ (Yerp _) = Nerp

identityLaw2 :: Poohoops String -> Bool
identityLaw2 j = j == fmap id j

-- identityLaw2 = False (mostly)

compositionLaw2 :: Poohoops String -> Bool
compositionLaw2 j =
  fmap (capitalise . exclaim) j
    == fmap capitalise (fmap exclaim j)
-- either way of doing this ends up the same
