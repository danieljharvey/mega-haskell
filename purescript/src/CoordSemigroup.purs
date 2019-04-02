module CoordSemigroup where

import Prelude

newtype Coord = Coord
    { x       :: Int
    , y       :: Int
    , offsetX :: Int
    , offsetY :: Int
    }

derive newtype instance eqCoord :: Eq Coord
derive newtype instance ordCoord :: Ord Coord
derive newtype instance showCoord :: Show Coord

instance semigroupCoord :: Semigroup Coord where
  append (Coord fst) (Coord snd)
    = Coord { x: fst.x + snd.x
            , y: fst.y + snd.y
            , offsetX: fst.offsetX + snd.offsetX
            , offsetY: fst.offsetY + snd.offsetY
            }

instance monoidCoord :: Monoid Coord where
  mempty = Coord { x: 0, y: 0, offsetX: 0, offsetY: 0 }

invert :: Coord -> Coord
invert (Coord coord)
  = Coord { x: (-1) * coord.x
          , y: (-1) * coord.y
          , offsetX: (-1) * coord.offsetX
          , offsetY: (-1) * coord.offsetY
          }

createCoord :: Int -> Int -> Coord
createCoord x y = Coord { x, y, offsetX: 0, offsetY: 0 }

createFullCoord :: Int -> Int -> Int -> Int -> Coord
createFullCoord x y offsetX offsetY
  = Coord { x, y, offsetX, offsetY }

createMoveCoord :: Int -> Coord -> Coord
createMoveCoord amount (Coord coord)
  = Coord { x: 0
          , y: 0
          , offsetX: amount * coord.x
          , offsetY: amount * coord.y
          }

--


type Player
  = { position  :: Coord
    , direction :: Coord
    }

newPlayer :: Player
newPlayer = { position  : mempty
            , direction : createCoord 1 0
            }

position :: Coord
position
  = Coord { x: 3
          , y: 2
          , offsetX: 0
          , offsetY: 32
          }

falling :: Coord
falling
  = Coord { x: 0
          , y: 1
          , offsetX: 0
          , offsetY: 0
          }

canIMoveDown :: Boolean
canIMoveDown = canMove (position <> falling)

canMove :: Coord -> Boolean
canMove _ = true

canIMoveNext :: Player -> Boolean
canIMoveNext player@{ position: Coord pos, direction: Coord dir }
  | dir.x < 0 = canMove ( Coord $ pos { x = pos.x - 1 } )
  | dir.x > 0 = canMove ( Coord $ pos { x = pos.x + 1 } )
  | dir.y < 0 = canMove ( Coord $ pos { y = pos.y - 1 } )
  | dir.y > 0 = canMove ( Coord $ pos { y = pos.y + 1 } )
  | otherwise       = true

canIMoveTwo :: Player -> Boolean
canIMoveTwo player
  = canMove (player.position <> player.direction)

turnAround :: Player -> Player
turnAround player
  = player { direction = invert player.direction }

updateDirection :: Player -> Player
updateDirection player
  = if canMove player.position
    then player
    else turnAround player

incrementPlayerPosition :: Int -> Player -> Player
incrementPlayerPosition moveAmount player@{ position: Coord pos, direction: Coord dir }
  = player { position = newPosition }
  where
    newPosition
      | dir.x < 0 = Coord $ pos { offsetX = pos.offsetX - moveAmount }
      | dir.x > 0 = Coord $ pos { offsetX = pos.offsetX + moveAmount }
      | dir.y < 0 = Coord $ pos { offsetY = pos.offsetY - moveAmount }
      | dir.y > 0 = Coord $ pos { offsetY = pos.offsetY + moveAmount }
      | otherwise = Coord pos


downMove :: Coord
downMove
  = Coord { x: 0
          , y: 0
          , offsetX: 0
          , offsetY: 20
          }

incrementPlayerDirection :: Int -> Player -> Player
incrementPlayerDirection amount player
  = player { position = newPosition }
  where
    newPosition :: Coord
    newPosition
      = player.position <> moveCoord

    moveCoord :: Coord
    moveCoord
      = createMoveCoord amount player.direction

doMove :: Int -> Player -> Player
doMove amount
  = updateDirection
  >>> incrementPlayerDirection amount
