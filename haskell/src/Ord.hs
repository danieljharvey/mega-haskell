module Ord where

import           Data.List

data Mood = Awful | QuiteBad | OK | Good | Great

-- broken :: Bool
-- broken = Awful < OK

data SuperMood = Worse | PrettyAverage | Fine deriving (Eq, Show)

instance Ord SuperMood where
    _ <= Fine = True
    Worse <= PrettyAverage = True
    _ <= _ = False

yep :: Bool
yep = Worse < PrettyAverage
-- yep = True

yep2 :: Bool
yep2 = Fine > Worse
-- yep2 = True

yep3 :: Bool
yep3 = Fine >= Fine
-- yep2 = True

nope :: Bool
nope = Fine < Fine
-- nope = False

moods :: [SuperMood]
moods = [Fine, Fine, Worse, PrettyAverage]

sorted :: [SuperMood]
sorted = sort moods

data LazyMood = Sloppy | Ploppy | Poopy | Nicey deriving (Eq, Ord, Show)

lazySorted :: [LazyMood]
lazySorted = sort [Nicey, Poopy, Ploppy, Sloppy]
-- lazySorted = [Sloppy, Ploppy, Poopy, Nicey]
