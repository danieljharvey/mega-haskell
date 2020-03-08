{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Control.Applicative (Alternative)
import qualified Data.Map as M
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Animal = Dog | Cat | Horse
  deriving (Eq, Ord, Show)

animal :: Parser Animal
animal =
  choice
    [ Dog <$ string' "dog",
      Cat <$ string' "cat",
      Horse <$ string' "horse"
    ]

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

animalWithVal :: Parser (M.Map Animal Int)
animalWithVal = do
  animal' <- animal
  _ <- allowSpace (char ':')
  num <- allowSpace $ label ("number of " <> show animal' <> "s") L.decimal
  pure (M.singleton animal' num)

allowSpace :: Parser a -> Parser a
allowSpace p = do
  _ <- sc
  a <- p
  _ <- sc
  pure a

animalMap :: Parser (M.Map Animal Int)
animalMap = do
  _ <- allowSpace $ char '{'
  val <- allowSpace $ sepBy1 animalWithVal (allowSpace $ char ',')
  _ <- allowSpace $ char '}'
  pure (mconcat (Prelude.reverse val))

test :: IO ()
test = parseTest animalMap "{dog:1, cat: 200, horse: 100}"
