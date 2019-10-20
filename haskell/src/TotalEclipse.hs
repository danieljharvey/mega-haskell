module TotalEclipse where

head :: [a] -> a
head (x : xs) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

data NonEmpty a = NonEmpty a [a]

nonEmptyHead :: NonEmpty a -> a
nonEmptyHead (NonEmpty x _) = x

createNonEmpty :: [a] -> Maybe (NonEmpty a)
createNonEmpty [] = Nothing
createNonEmpty (x : xs) = Just (NonEmpty x xs)
