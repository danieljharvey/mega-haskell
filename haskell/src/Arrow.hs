{-# LANGUAGE Arrows #-}

module Arrow where

import qualified Control.Arrow as Arr
import Control.Category
import Data.Maybe
import Prelude hiding ((.), id)

newtype SimpleFunc a b
  = SimpleFunc {runF :: a -> b}

instance Arr.Arrow SimpleFunc where

  arr = SimpleFunc

  first (SimpleFunc f) = SimpleFunc (mapFst f)
    where
      mapFst g (a, b) = (g a, b)

  second (SimpleFunc f) = SimpleFunc (mapSnd f)
    where
      mapSnd g (a, b) = (a, g b)

instance Category SimpleFunc where

  (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)

  id = Arr.arr id

data Animal
  = Cat
  | Dog
  | Horse
  | Other
  deriving (Show, Eq, Ord)

readAnimal :: String -> Animal
readAnimal "cat" = Cat
readAnimal "dog" = Dog
readAnimal "horse" = Horse
readAnimal _ = Other

cycleAnimal :: Animal -> Animal
cycleAnimal Cat = Dog
cycleAnimal Dog = Horse
cycleAnimal Horse = Cat
cycleAnimal Other = Other

readAnimalA :: SimpleFunc String Animal
readAnimalA = Arr.arr readAnimal

cycleAnimalA :: SimpleFunc Animal Animal
cycleAnimalA = Arr.arr cycleAnimal

animalTimeA :: SimpleFunc String (Animal, Animal)
animalTimeA = proc str -> do
  animal <- readAnimalA -< str
  nextAnimal <- cycleAnimalA -< animal
  Arr.returnA -< (animal, nextAnimal)

pairOfPets :: (Animal, Animal)
pairOfPets = runF animalTimeA "dog"

askQuestion :: String -> IO String
askQuestion s = do
  putStrLn s
  getLine

askQuestionK :: Arr.Kleisli IO String String
askQuestionK = Arr.Kleisli askQuestion

manyQuestionsK :: Arr.Kleisli IO String Int
manyQuestionsK = proc str -> do
  first <- askQuestionK -< ("1. " ++ str)
  second <- askQuestionK -< ("2. " ++ str)
  third <- askQuestionK -< ("3. " ++ str)
  fourth <-
    if third == "dog"
      then do
        fourth' <- askQuestionK -< ("Bonus. " ++ str)
        Arr.returnA -< Just fourth'
      else Arr.returnA -< Nothing
  Arr.returnA -<
    isDog first
      + isDog second
      + isDog third
      + maybe 0 isDog fourth
  where
    isDog :: String -> Int
    isDog s = if s == "dog" then 1 else 0

questionTime :: IO ()
questionTime = do
  i <- Arr.runKleisli manyQuestionsK "Type dog"
  putStrLn $ "You typed 'dog' " ++ show i ++ " times."
