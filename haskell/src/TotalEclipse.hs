module TotalEclipse where

head :: [a] -> a
head (x : _) = x
head [] = undefined

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

data NonEmpty a = NonEmpty a [a]

nonEmptyHead :: NonEmpty a -> a
nonEmptyHead (NonEmpty x _) = x

createNonEmpty :: [a] -> Maybe (NonEmpty a)
createNonEmpty [] = Nothing
createNonEmpty (x : xs) = Just (NonEmpty x xs)
