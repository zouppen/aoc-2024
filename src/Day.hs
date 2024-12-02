module Day where

import Data.Attoparsec.Text (Parser)
import System.IO (Handle)

data Solver a b = ShowSolver (a -> b)
                | StringSolver (a -> String)
                | HandleSolver (Handle -> a -> IO ())

data Day a b = Day { parser  :: Parser a
                   , solvers :: [(String, Solver a b)]
                   }
