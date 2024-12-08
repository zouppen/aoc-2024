module Day where

import Data.Attoparsec.ByteString (Parser)

data Solver a b = ShowSolver (a -> b)
                | StringSolver (a -> String)

data Day a b = Day { parser  :: Parser a
                   , solvers :: [(String, Solver a b)]
                   }

part1 :: (a -> b) -> (String, Solver a b)
part1 x = ("part1", ShowSolver x)

part2 :: (a -> b) -> (String, Solver a b)
part2 x = ("part1", ShowSolver x)
