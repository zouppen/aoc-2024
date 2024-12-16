{-# LANGUAGE TupleSections #-}
module Day11 where

import Data.Attoparsec.ByteString.Char8
import qualified Data.IntMap as IM
import Data.Bits ((.>>.))

import Day

task :: Day [Int] Int
task = Day { parser  = parseAll
           , solvers = [ part 1 $ blinkGeneration 25
                       , part 2 $ blinkGeneration 75
                       ]
           }

-- Super simple parsing

parseAll :: Parser [Int]
parseAll = do
  list <- decimal `sepBy` char ' '
  endOfLine
  endOfInput
  pure list

-- And epic math

blinkGeneration :: Int -> [Int] -> Int
blinkGeneration n xs = sum $ iterate nextGen firstGen !! n
  where firstGen = IM.fromList $ map (,1) xs
        nextGen = IM.fromListWith (+) . kaboom . IM.toList

kaboom :: [(Int, a)] -> [(Int, a)]
kaboom xs = [ (new, n)
            | (old, n) <- xs
            , new <- blink old
            ]

blink :: Int -> [Int]
blink x | x == 0      = [1]
        | even digits = [upper, lower]
        | otherwise   = [2024*x]
  where digits = ceilLog10 x
        (upper, lower) = quotRem x (10^(digits .>>. 1))

-- |10-base logarithm rounded up, i.e. digit count.
ceilLog10 :: Int -> Int
ceilLog10 x | x < 1     = error "Operand not positive"
            | otherwise = f 10 1 x
  where f n logN a = if a < n
                     then logN
                     else f (10*n) (logN+1) a
