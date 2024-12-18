{-# LANGUAGE OverloadedStrings #-}
module Day ( Solver(..)
           , Day(..)
           , part
           -- Re-exports to avoid including those every day
           , Generic
           , ToJSON
           , toJSON
           ) where

import Data.Aeson (ToJSON, toJSON)
import Data.Attoparsec.ByteString (Parser)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data Solver a b = ShowSolver (a -> b)
                | StringSolver (a -> String)

data Day a b = Day { parser  :: Parser a
                   , solvers :: [(Text, Solver a b)]
                   }

-- Shortcuts for defining the parts in individual day modules

part :: Int -> (a -> b) -> (Text, Solver a b)
part n x = (pack ("part" <> show n), ShowSolver x)
