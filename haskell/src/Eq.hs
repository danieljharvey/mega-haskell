module Eq where

data Horse = SmallHorse | LargeHorse | OtherHorse

-- isSameHorse :: Horse -> Horse -> Bool
-- isSameHorse firstHorse secondHorse = firstHorse == secondHorse

data BetterHorse = Tiny | Average | Huge

instance Eq BetterHorse where
  Tiny == Tiny = True
  Average == Average = True
  Huge == Huge = True
  _ == _ = False

isSameBetterHorse :: BetterHorse -> BetterHorse -> Bool
isSameBetterHorse firstHorse secondHorse = firstHorse == secondHorse

nope :: Bool
nope = isSameBetterHorse Tiny Huge

yep :: Bool
yep = isSameBetterHorse Average Average

data LazyHorse = LazyTiny | LazyOther deriving (Eq)

workingNow = LazyTiny == LazyOther
