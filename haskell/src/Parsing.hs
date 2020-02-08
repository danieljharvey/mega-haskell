{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.Text
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

what = parseTest (satisfy (== 'a') :: Parser Char) ""
