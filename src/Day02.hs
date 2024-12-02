module Day02 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.List (group)

import Day

task :: Day [[Int]] Int
task = Day { parser = everything
           , solvers = [("part1", ShowSolver $ length . filter isGoodRow)
                       ,("part2", ShowSolver $ length . filter isGoodRow2)
                       ]
           }

-- Parsing tools

everything :: Parser [[Int]]
everything = many line <* endOfInput

line :: Parser [Int]
line = decimal `sepBy` char ' ' <* endOfLine

-- Solving

isGoodRow :: [Int] -> Bool
isGoodRow xs = isInRange xs && isMonotonic xs

diffs :: [Int] -> [Int]
diffs row = zipWith (-) row (tail row)

isInRange :: [Int] -> Bool
isInRange row = all (fine.abs) $ diffs row
  where fine x = x >= 1 && x <= 3

isMonotonic :: [Int] -> Bool
isMonotonic row = case (group $ map signum $ diffs row) of
  [_] -> True
  _   -> False

isGoodRow2 :: [Int] -> Bool
isGoodRow2 xs = any isGoodRow $ dampenerify [] xs

dampenerify :: [a] -> [a] -> [[a]]
dampenerify start (x:xs) = (start <> xs) : dampenerify (start <> pure x) xs
dampenerify start [] = pure start
