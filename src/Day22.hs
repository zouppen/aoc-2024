{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Day22 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, endOfInput)
import qualified Data.Map.Strict as M
import Data.Bits

import Day

task :: Day [Int] Int
task = Day { parser  = many (decimal <* endOfLine) <* endOfInput
           , solvers = [ part 1 $ sum . map (last . pseudoIter 2000)
                       , part 2 $ maximum . process 2000
                       ]
           }

-- |Pseudorandom algorithm described in the puzzle. Replaced
-- multiplications, divisions and modulos with bitwise operations.
pseudorandom :: Int -> Int
pseudorandom = f3 . f2 . f1
  where f1 x = prune $ mix x $ x .<<. 6
        f2 x = prune $ mix x $ x .>>. 5
        f3 x = prune $ mix x $ x .<<. 11
        prune x = x .&. 0xffffff
        mix a b = a `xor` b

pseudoIter :: Int -> Int -> [Int]
pseudoIter n = take (n+1) . iterate pseudorandom

-- |Collect values where two numbers are equal so that there are 2 any
-- numbers in between. Return the differences to the previous values
-- and the ending number.
candidates :: (Eq a, Num a) => [a] -> [([a], a)]
candidates (a:rest@(b:c:d:e:_)) =
  if b == e
  then ([a-b, b-c, c-d, d-e], e) : candidates rest
  else candidates rest
candidates _ = []

secretToPrice :: Integral a => a -> a
secretToPrice secret = secret `rem` 10

-- |Convert the price list to map of sequences and drop the
-- later-occurring candidates of the same sequence. (i.e. accept only
-- one unique sequence per price list).
candidateMap :: [Int] -> M.Map [Int] Int
candidateMap = M.fromListWith (flip const) .
               candidates . map secretToPrice

-- |Calculate total hit count for each seed over the whole data set.
process :: Int -> [Int] -> M.Map [Int] Int
process n = M.unionsWith (+) . map (candidateMap . pseudoIter n)
