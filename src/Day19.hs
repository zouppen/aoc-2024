{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Day19 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.IntMap as IM
import Data.List (isPrefixOf, tails, sortOn)

import Day

task :: Day Puzzle Int
task = Day { parser  = everything
           , solvers = [ part 1 $ length . filter (/= 0) . routeCounts
                       , part 2 $ sum . routeCounts
                       ]
           }

newtype Towel = Towel [Colour] deriving (Generic, Show)

data Colour = White | Blue | Black | Red | Green deriving (Generic, Show, Eq)

data Puzzle = Puzzle { towels   :: [Towel]
                     , patterns :: [[Colour]]
                     } deriving (Generic, Show)

instance ToJSON Puzzle
instance ToJSON Colour
instance ToJSON Towel

-- Parsers

everything :: Parser Puzzle
everything = do
  towels <- (Towel <$> colourRun) `sepBy` (string ", ")
  _ <- count 2 endOfLine
  patterns <- many $ colourRun <* endOfLine
  endOfInput
  pure Puzzle{..}

colourRun :: Parser [Colour]
colourRun = many1 colour

colour :: Parser Colour
colour = do
  c <- anyChar
  case c of
    'w' -> pure White
    'u' -> pure Blue
    'b' -> pure Black
    'r' -> pure Red
    'g' -> pure Green
    _   -> empty

-- Business as usual. Part 1 was really faster with regex, but after
-- getting the star I wrote the this tring here to not to "cheat" by
-- skipping the Haskell fun.

routeCount :: [Towel] -> [Colour] -> Int
routeCount towels colours = IM.findWithDefault 0 0 m
  where hops = hopGraph towels colours
        m = routeMap (length colours) hops

hopGraph :: [Towel] -> [Colour] -> [(Int, Int)]
hopGraph towels colours = [ (n, n + length towel)
                          | (n, xs) <- zip [0..] $ tails colours
                          , Towel towel <- towels
                          , towel `isPrefixOf` xs
                          ]

routeMap :: Int -> [(Int, Int)] -> IM.IntMap Int
routeMap goal hops = foldl summer initial lastFirst
  where lastFirst = sortOn (negate.snd) hops
        initial = IM.singleton goal 1 -- One route to goal
        summer m (src, tgt) = let curVal = IM.findWithDefault 0 tgt m
                              in IM.insertWith (+) src curVal m

routeCounts :: Puzzle -> [Int]
routeCounts p = map (routeCount $ towels p) (patterns p)
