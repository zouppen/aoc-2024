-- |Parsers for elements which Wastl often puts in his puzzles.
module Wastl ( direction
             , ignoreNewline
             ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

import Grid (Coord)

direction :: Parser Coord
direction = anyChar >>= \c -> case c of
  '<' -> pure (-1,  0)
  '>' -> pure ( 1,  0)
  '^' -> pure ( 0, -1)
  'v' -> pure ( 0,  1)
  _   -> empty

-- |Wraps a parser to ignore a single line break in the middle of the
-- data.
ignoreNewline :: Parser p -> Parser p
ignoreNewline p = p <|> (endOfLine >> p)
