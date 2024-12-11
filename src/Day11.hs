{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Day11 where

import Data.Attoparsec.ByteString.Char8
import qualified Data.IntMap as IM

import Day

task :: Day [Int] Int
task = Day { parser  = parseAll
           , solvers = [ part1 $ \p -> sum $ IM.elems $ (iterate aatto $ IM.fromList (map (,1) p)) !! 25
                       , part2 $ \p -> sum $ IM.elems $ (iterate aatto $ IM.fromList (map (,1) p)) !! 75
                       ]
           }

parseAll :: Parser [Int]
parseAll = do
  list <- decimal `sepBy` char ' '
  endOfLine
  endOfInput
  pure list

tuho :: (Int, t) -> [(Int, t)]
tuho (x, n) = map (, n) $ bl x

aatto :: Num a => IM.IntMap a -> IM.IntMap a
aatto m = IM.fromListWith (+) $ concatMap tuho $ IM.toList m

bl :: Int -> [Int]
bl x = if x == 0
       then [1]
       else if mod (length intDigits) 2 == 0
            then (\(a, b) -> [read a, read b]) $ splitAt (length intDigits `div` 2) intDigits
            else [2024*x]
   where intDigits = show x
