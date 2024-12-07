{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day07 where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Aeson as A
import GHC.Generics

import Day

task :: Day [Row] Integer
task = Day { parser = everything
           , solvers = [("part1", ShowSolver $ sumPossibles [(+), (*)])
                       ,("part2", ShowSolver $ sumPossibles [(+), (*), cc])
                       ]
           }

data Row = Row { expected :: Integer
               , operands :: [Integer]
               } deriving (Show, Eq, Generic)

instance A.ToJSON Row

everything :: Parser [Row]
everything = many row <* endOfInput

row :: Parser Row
row = do
  expected <- decimal
  _ <- string ": "
  operands <- decimal `sepBy` char ' '
  endOfLine
  pure Row{..}

sumPossibles :: [Integer -> Integer -> Integer] -> [Row] -> Integer
sumPossibles ops = sum . map expected . filter (isPossible ops)

-- |Returns 0 if Row expected result can't be c
isPossible :: [Integer -> Integer -> Integer] -> Row -> Bool
isPossible ops Row{..} = any isMatch $ evalLTR ops expected operands
  where isMatch res = expected == res

-- |Give possible evaluations, with given operator set and evaluate
-- from left to right.
evalLTR :: Ord a => [a -> a -> a] -> a -> [a] -> [a]
evalLTR ops cap = evalRTL ops cap . reverse

-- |Give possible evaluations, with given operator set and evaluate
-- from right to left.
evalRTL :: Ord a => [a -> a -> a] -> a -> [a] -> [a]
evalRTL _ _ [a] = [a]
evalRTL ops cap (rval:xs) = do
  lval <- evalRTL ops cap xs
  op <- ops
  let res = lval `op` rval
  guard $ res <= cap -- Optimization: If value is already too high
  pure res
evalRTL _ _ [] = []

cc :: (Show a, Read b) => a -> a -> b
cc a b = read $ show a <> show b
