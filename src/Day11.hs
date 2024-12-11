{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Day11 where

import Data.Attoparsec.ByteString.Char8
import qualified Data.IntMap as IM

import Day

task :: Day [Int] Int
task = Day { parser  = parseAll
           , solvers = [ part1 $ iterateBlinkTo 25
                       , part2 $ iterateBlinkTo 75
                       ]
           }

parseAll :: Parser [Int]
parseAll = do
  list <- decimal `sepBy` char ' '
  endOfLine
  endOfInput
  pure list

iterateBlinkTo :: Int -> [Int] -> Int
iterateBlinkTo n xs = sum $ IM.elems $ (iterate aatto $ IM.fromList (map (,1) xs)) !! n

tuho :: (Int, t) -> [(Int, t)]
tuho (x, n) = map (, n) $ blink x

aatto :: Num a => IM.IntMap a -> IM.IntMap a
aatto m = IM.fromListWith (+) $ concatMap tuho $ IM.toList m

blink :: Int -> [Int]
blink x | x == 0      = [1]
        | oddity == 0 = (\(a, b) -> [read a, read b]) $ splitAt pos intDigits
        | otherwise   = [2024*x]
  where intDigits = show x
        (pos, oddity) = quotRem (length intDigits) 2
