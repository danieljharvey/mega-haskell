{-# LANGUAGE DeriveFunctor #-}
module Comonad where

import           Control.Comonad
import           Control.Comonad.Store
import           Data.Monoid

data Which = This | That | Who deriving (Show, Eq)

getStuff :: Which -> String
getStuff This = "this thing"
getStuff That = "that other thing"
getStuff Who  = "who the fuck?"

stuffStore :: Store Which String
stuffStore = store getStuff This

  {-
(f,a) = runStore stuffStore

output :: String
output = f a

other :: String
other = f That
-}

currentPosition :: Which
currentPosition = pos stuffStore

somethingElse :: String
somethingElse = peek That stuffStore

invert :: Which -> Which
invert This = That
invert That = Who
invert Who  = This

swapped :: String
swapped = peeks invert stuffStore

focusedOnThat :: Store Which String
focusedOnThat = seek That stuffStore

itsThat :: Which
itsThat = pos focusedOnThat

nowItsWho :: Which
nowItsWho = pos $ seeks invert focusedOnThat

-- experiment :: Functor f => (s -> f s) -> w a -> f a

thisOrThat :: Which -> Maybe Which
thisOrThat Who = Nothing
thisOrThat w   = Just w

-- experiment is 'functor map over key to get fuc
what :: Maybe String
what = experiment thisOrThat stuffStore

otherWhat :: Maybe String
otherWhat = experiment thisOrThat $ seeks invert stuffStore

otherOtherWhat :: Maybe String
otherOtherWhat = experiment thisOrThat
    $ seeks invert $ seeks invert stuffStore

{-
 oh my god what are we even doing, this seems so much work,

 lets try a more interesting key maybe?
-}

data Slice = First | Middle | Last deriving (Eq, Show)

type Location = (Slice, Slice)

data BattenType = Pink | Other deriving (Eq, Show)

getBattenType :: Location -> BattenType
getBattenType (x, y) = if x == y then Pink else Other

{- yeah -}

type Grid = [[ Bool ]]

startGrid :: Grid
startGrid = [ [True , False, False, True, False]
            , [True , False, False, True, False]
            , [False, False, False, True, False]
            , [False, True,  False, True, False]
            , [False, False, False, True, False]
            ]

gridHeight :: Grid -> Int
gridHeight grid
  = length grid

gridWidth :: Grid -> Int
gridWidth []    = 0
gridWidth (a:_) = length a

type Point = (Int, Int)

startPoint :: Point
startPoint = (2,2)

getGridItem :: Grid -> Point -> Bool
getGridItem grid (x,y)
  = if withinBounds
       then item
       else False
  where
    withinBounds
      =  (x > -1)
      && (x < gridWidth grid)
      && (y > -1)
      && (y < gridHeight grid)
    item
      = (grid !! y) !! x

startStore :: Store Point Bool
startStore
  = store (getGridItem startGrid) startPoint

firstItem :: Bool
firstItem = extract startStore
-- firstItem == False

-- adds up all of the items around our item
nextStep :: Store Point Bool -> Int
nextStep store'
  = foldr (+) 0 [ look (-1) (-1) , look 0 (-1) , look 1 (-1)
                , look (-1) 0    , look 0 0    , look 1 0
                , look (-1) 1    , look 0 1    , look 1 1
                ]
    where
      look :: Int -> Int -> Int
      look x y
        = if peeks (\(a,b) -> (x + a, y + b)) store'
          then 1
          else 0

-- our startStore, after nextStep has been run over each item
endStore :: Store Point Int
endStore = extend nextStep startStore

secondItem :: Int
secondItem = extract endStore
-- secondItem == 4

thirdItem :: Int
thirdItem = peek (0,1) endStore
-- thirdItem == 2

fourthItem :: Int
fourthItem = peek (0,0) endStore
-- fourthItem == 2

countMines :: Store Point Bool -> Point -> Int
countMines store' point
  = peek point (extend nextStep store')

{-
class Functor w => Comonad (w :: * -> *) where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  {-# MINIMAL extract, (duplicate | extend) #-}
-}

data MyNonEmpty a
  = a :| [a]
  deriving (Show, Ord, Eq)

infixr 5 :|

singleton :: a -> MyNonEmpty a
singleton item = item :| []

toList :: MyNonEmpty a -> [a]
toList (a :| as) = a : as

instance Functor MyNonEmpty where
  fmap f (a :| as)
    = (f a) :| (f <$> as)

instance Comonad MyNonEmpty where
  extract (a :| _) = a

  duplicate allTheThings@(_ :| [])
    = allTheThings :| []

  duplicate allTheThings@(_ :| (b : bs))
    = allTheThings :| (toList . duplicate $ (b :| bs))

-- defer to list fold
instance Foldable MyNonEmpty where
  foldr f def as = foldr f def (toList as)

niceList :: MyNonEmpty Int
niceList = 1 :| [2,3,4,5,6,7]

productList :: MyNonEmpty Int
productList
  = extend f niceList
  where
    f as =
      getProduct $ foldMap Product as
-- productList == 5040 :| [5040,2520,840,210,42,7]

sumList :: MyNonEmpty Int
sumList
  = extend f niceList
  where
    f as
      = getSum $ foldMap Sum as
-- sumList == 28 :| [27,25,22,18,13,7]

data Pet
  = Horse
  deriving (Show)

data Event
  = OnBlur
  | OnFocus
  deriving (Eq, Ord, Show)

changeStream :: MyNonEmpty String
changeStream = "h" :| [ "ho", "hor", "hors", "horse", "horses" ]

eventStream :: [Event]
eventStream = [ OnBlur, OnFocus, OnFocus ]

hasBlurred :: [Event] -> Bool
hasBlurred = foldl (\as a -> as || a == OnBlur) False

-- bad comonad
getPet :: MyNonEmpty String -> Maybe Pet
getPet = foldr (\s _ -> case s of
             "horse" -> Just Horse
             _       -> Nothing) Nothing

validated :: MyNonEmpty (Maybe Pet)
validated = extend getPet changeStream

data Zipper a
  = Zipper
      { prev  :: [a]
      , value ::  a
      , next  :: [a]
      }
  deriving (Eq, Ord, Show, Functor)

lastMaybe :: [a] -> Maybe a
lastMaybe (x : []) = Just x
lastMaybe (_ : xs) = lastMaybe xs
lastMaybe _        = Nothing

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (a : _) = Just a

shuffleLeft :: Zipper a -> Zipper a
shuffleLeft a
  = case lastMaybe (prev a) of
      Just prevItem ->
        Zipper
           { prev = init (prev a)
           , value = prevItem
           , next = (pure (value a)) <> next a
           }
      _ -> a

shuffleRight :: Zipper a -> Zipper a
shuffleRight a
  = case headMaybe (next a) of
      Just nextItem ->
        Zipper
           { prev = (prev a) <> (pure $ value a)
           , value = nextItem
           , next = drop 1 (next a)
           }
      _ -> a

startZip :: Zipper Int
startZip
  = Zipper
      { prev  = []
      , value = 1
      , next  = [2,3]
      }

endZip :: Zipper Int
endZip = (applyNTimes 3 shuffleRight startZip)

-- $> (shuffleLeft startZip) == startZip

-- $> shuffleRight startZip

-- $> applyNTimes 2 shuffleRight startZip == endZip

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f
  = foldr (.) id (replicate n f)

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]

instance Comonad Zipper where
  extract = value

  duplicate a
    = Zipper
        { prev  = mapInd (\_ i -> (applyNTimes i shuffleLeft) a) (prev a)
        , value = a
        , next  = mapInd (\_ i -> (applyNTimes i shuffleRight) a) (next a)
        }

duplicated :: Zipper (Zipper Int)
duplicated = extend id startZip
-- $> duplicated


