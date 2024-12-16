{-# LANGUAGE TupleSections #-}
module Day01 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.List (sort)
import qualified Data.IntMap.Strict as IM

import Day

task :: Day ([Int], [Int]) Int
task = Day { parser  = everything
           , solvers = [ part 1 doPart1
                       , part 2 doPart2
                       ]
           }

-- Parsing tools

everything :: Parser ([Int], [Int])
everything = do
  raw <- many pair
  endOfInput
  pure (map fst raw, map snd raw)

pair :: Parser (Int, Int)
pair = do
  pukki <- decimal
  skipSpace
  muori <- decimal
  endOfLine
  pure (pukki, muori)

-- Solving

doPart1 :: ([Int], [Int]) -> Int
doPart1 (pukit, muorit) = sum $ zipWith dist (sort pukit) (sort muorit)
  where dist pukki muori = abs $ pukki - muori

doPart2 :: ([Int], [Int]) -> Int
doPart2 (pukit, muorit) = sum $ map tonttu pukit
  where himmeli = IM.fromListWith (+) $ map (,1) muorit
        tonttu pukki = pukki * IM.findWithDefault 0 pukki himmeli
