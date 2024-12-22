{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Day22 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, endOfInput)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bits

import Day

task :: Day [Int] Int
task = Day { parser  = many (decimal <* endOfLine) <* endOfInput
           , solvers = [ part 2 $ maximum . process
                       ]
           }

pseudorand :: Int -> Int
pseudorand = f3 . f2 . f1
  where f1 x = prune $ mix x $ x .<<. 6
        f2 x = prune $ mix x $ x .>>. 5
        f3 x = prune $ mix x $ x .<<. 11
        prune x = x .&. 0xffffff
        mix a b = a `xor` b

-- |Collect values where two numbers are equal so that there are 2 any
-- numbers in between. Return the differences to the previous values
-- and the ending number.
candidates :: (Eq b, Num b) => [b] -> [([b], b)]
candidates (a:rest@(b:c:d:e:_)) =
  if b == e
  then ([a-b, b-c, c-d, d-e], e) : candidates rest
  else candidates rest
candidates _ = []

secretToPrice :: Integral a => a -> a
secretToPrice secret = secret `rem` 10

-- |Collect candidates and drop the later-occuring candidates with the
-- same sequence.
candidateMap :: Int -> M.Map [Int] Int
candidateMap = M.fromListWith (flip const) .
               candidates . map secretToPrice .
               take 2001 . iterate pseudorand

process :: [Int] -> [Int]
process secrets = map poimi $ S.toList allKeys
  where
    maps = map candidateMap secrets
    allKeys = S.unions $ map M.keysSet maps
    poimi sequ = sum $ map (kurkkaa sequ) maps
    kurkkaa sequ m = M.findWithDefault 0 sequ m
