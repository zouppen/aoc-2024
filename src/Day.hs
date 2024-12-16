{-# LANGUAGE OverloadedStrings #-}
module Day ( Solver(..)
           , Day(..)
           , part1
           , part2
           -- Re-exports to avoid including those every day
           , Generic
           , ToJSON
           ) where

import Data.Aeson (ToJSON)
import Data.Attoparsec.ByteString (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

data Solver a b = ShowSolver (a -> b)
                | StringSolver (a -> String)

data Day a b = Day { parser  :: Parser a
                   , solvers :: [(Text, Solver a b)]
                   }

-- Shortcuts for defining the parts in individual day modules

part1 :: (a -> b) -> (Text, Solver a b)
part1 x = ("part1", ShowSolver x)

part2 :: (a -> b) -> (Text, Solver a b)
part2 x = ("part2", ShowSolver x)
