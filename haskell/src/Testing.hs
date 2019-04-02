module Testing where

import Control.Monad.Identity

import qualified Data.Time.Clock     as Clock
import qualified Data.Time.Calendar  as Cal
import qualified Data.Time.LocalTime as Time

-- examples

identity :: a -> a
identity a = a
-- identity 10 == 10

const :: a -> b -> a
const a _ = a
-- const "dog" 100 == "dog"

showTheList :: Show a => [a] -> String
showTheList []       = ""
showTheList (a : as) = show a ++ ", " ++ showTheList as
-- showTheList [1,2,3] == "1, 2, 3, "

addStuffUp :: Num a => [a] -> a
addStuffUp []       = 0
addStuffUp (a : as) = a + addStuffUp as
-- addStuffUp [1,2,3] == 6


baseTestTime :: Clock.UTCTime
baseTestTime
  = Clock.UTCTime
      { Clock.utctDay     = Cal.ModifiedJulianDay 12000
      , Clock.utctDayTime = 0
      }

lunchTestTime :: Clock.UTCTime
lunchTestTime
  = baseTestTime { Clock.utctDayTime = 44000 }

endLunchTestTime :: Clock.UTCTime
endLunchTestTime
  = baseTestTime { Clock.utctDayTime = 52000 }

type Hour = Int

getHour :: Clock.UTCTime -> Hour
getHour = Time.todHour . Time.timeToTimeOfDay . Clock.utctDayTime

-- simplest version
isItLunchTime :: IO Bool
isItLunchTime
  = lunchCheck <$> getHour <$> Clock.getCurrentTime
    where
      lunchCheck hr = hr >= 12 && hr <= 14

-- let's have the IO function passed in instead
injectableLunch :: IO Clock.UTCTime -> IO Bool
injectableLunch getTime
  = lunchCheck <$> getHour <$> getTime
    where
      lunchCheck hr = hr >= 12 && hr <= 14

-- and then generalise the
-- let's break out most of the logic into this polymorphic version
testableLunch :: (Monad m) => m Clock.UTCTime -> m Bool
testableLunch getTime
  = lunchCheck <$> getHour <$> getTime
    where
      lunchCheck hr = hr >= 12 && hr <= 14

-- now we can test this code with a much safer monad
testNotLunch :: Identity Bool
testNotLunch = testableLunch (pure baseTestTime)
-- Identity False

-- now we can test this code with a much safer monad
testIsLunch :: Identity Bool
testIsLunch = testableLunch (pure lunchTestTime)
-- Identity True

-- now we have a new version of the function to use in code
-- which is covered in tests
isItLunchTime3 :: IO Bool
isItLunchTime3 = testableLunch Clock.getCurrentTime


-- another version
class Monad m => MonadTime m where
  getTheTimePlease :: m Clock.UTCTime

-- for REAL
instance MonadTime IO where
  getTheTimePlease = Clock.getCurrentTime

-- for testing
instance MonadTime Identity where
  getTheTimePlease = pure lunchTestTime

-- this version uses the typeclasses instead of injection
classyLunch :: (MonadTime m) => m Bool
classyLunch
  = lunchCheck <$> getHour <$> getTheTimePlease
    where
      lunchCheck hr = hr >= 12 && hr <= 14

testClassyLunch :: Identity Bool
testClassyLunch = classyLunch

ioClassyLunch :: IO Bool
ioClassyLunch = classyLunch
