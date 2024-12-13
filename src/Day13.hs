{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Day11 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.IntMap as IM
import Data.Bits ((.>>.))
import GHC.Generics

import Day

import Tonttu

type Coord = (Int, Int)

data Machine = Machine { deltaA :: Coord
                       , deltaB :: Coord
                       , winPos :: Coord
                       } deriving (Show, Generic)

machines :: Parser [Machine]
machines = many machineParse <* endOfInput

machineParse :: Parser Machine
machineParse = do
   deltaA <- buttonParse 'A'
   deltaB <- buttonParse 'B'
   winPos <- prizeParse
   _ <- many endOfLine
   pure Machine{..}

buttonParse c = do
  _ <- string "Button "
  _ <- char c
  _ <- string ": X"
  x <- signed decimal
  _ <- string ", Y"
  y <- signed decimal
  endOfLine
  pure (x, y)

prizeParse = do
  string "Prize: X="
  x <- decimal
  string ", Y="
  y <- decimal
  endOfLine
  pure (x, y)

-- The thing

priceA = 3
priceB = 1

-- |Gives maximum times a could fit in b
maxMult :: Integral a => (a, a) -> (a, a) -> a
maxMult (x1, y1) (x2, y2) = min (x2 `div` x1) (y2 `div` y1)

diff :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
diff (x1, y1) (x2, y2) = (x1-x2, y1-y2)

plus (x1, y1) (x2, y2) = (x1+x2, y1+y2)

mult :: Num a => a -> (a, a) -> (a, a)
mult n (x, y) = (n*x, n*y)

getSolutions :: Machine -> [(Int, Int)]
getSolutions Machine{..} = [ (aTimes, bTimes)
                           | aTimes <- [0..maxMult deltaA winPos]
                           , bTimes <- [0..maxMult deltaB (diff winPos (mult aTimes deltaA))]
                           , plus (mult aTimes deltaA) (mult bTimes deltaB) == winPos
                           ]

solutionPrice :: Num a => Machine -> (a, a) -> a
solutionPrice Machine{..} (moveA, moveB) = 3 * moveA + moveB


getCheapestSolution m = case map (solutionPrice m) $ getSolutions m of
  [] -> 0
  a  -> minimum a

-- pt1: sum $ map getCheapestSolution p
