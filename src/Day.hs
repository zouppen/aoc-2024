{-# LANGUAGE OverloadedStrings #-}
module Day where

import Data.Attoparsec.ByteString (Parser)
import Data.Text (Text)

data Solver a b = ShowSolver (a -> b)
                | StringSolver (a -> String)

data Day a b = Day { parser  :: Parser a
                   , solvers :: [(Text, Solver a b)]
                   }

part1 :: (a -> b) -> (Text, Solver a b)
part1 x = ("part1", ShowSolver x)

part2 :: (a -> b) -> (Text, Solver a b)
part2 x = ("part1", ShowSolver x)
