{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Day13 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

import Day
import AocTools.Grid (Coord)

task :: Day [Machine] Int
task = Day { parser  = machines
           , solvers = [ part 1 $ sum . map (toCost . findCounts)
                       , part 1 $ sum . map (toCost . findCounts. toPart2)
                       ]
           }

data Machine = Machine { deltaA :: Coord
                       , deltaB :: Coord
                       , prize  :: Coord
                       } deriving (Show, Generic)

instance ToJSON Machine

-- Parsing

machines :: Parser [Machine]
machines = many machine <* endOfInput

machine :: Parser Machine
machine = do
   deltaA <- buttonWith 'A'
   deltaB <- buttonWith 'B'
   prize  <- prizeParse
   _ <- many endOfLine
   pure Machine{..}

buttonWith :: Char -> Parser Coord
buttonWith c = do
  _ <- string "Button "
  _ <- char c
  _ <- string ": X"
  x <- signed decimal
  _ <- string ", Y"
  y <- signed decimal
  endOfLine
  pure (x, y)

prizeParse :: Parser Coord
prizeParse = do
  _ <- string "Prize: X="
  x <- decimal
  _ <- string ", Y="
  y <- decimal
  endOfLine
  pure (x, y)

-- Epic euclidean geometry skills

toPart2 :: Machine -> Machine
toPart2 m = m{ prize = op (10000000000000 +) (prize m) }
  where op f (x, y) = (f x, f y)

-- |Find x coordinate of intersection of two lines, deltaA going via
-- from origo and deltaB going via prize point.
intersectionX :: Machine -> Rational
intersectionX Machine{..} = (bB - bA) / (aA - aB)
  where
    -- Just to make code readable
    x = fromIntegral . fst
    y = fromIntegral . snd
    -- Preliminary results
    aA = y deltaA / x deltaA
    aB = y deltaB / x deltaB
    bA = 0
    bB = y prize - aB * x prize

findCounts :: Integral a => Machine -> Maybe (a, a)
findCounts m@Machine{..} = if isIntegral aCount && isIntegral bCount
                           then Just (floor aCount, floor bCount)
                           else Nothing
  where x = fromIntegral . fst
        midX = intersectionX m
        aCount = midX / x deltaA
        bCount = (x prize - midX) / x deltaB
        isIntegral a = fromIntegral (floor a :: Int) == a

toCost :: Num a => Maybe (a, a) -> a
toCost (Just (a, b)) = 3*a + b
toCost Nothing       = 0
