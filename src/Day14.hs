{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Day14 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take, takeWhile)
import Data.Bits
import qualified Data.Set as S
import Data.List (group, sort)
import Data.Maybe (catMaybes)

import Day
import AocTools.Grid

task :: Day (Grid [Robot]) Int
task = Day { parser  = robotArena areaProd
           , solvers = [ part 1 part1Magic
                       , part 2 $ findTheNonOverlapping . uncycle . iterate generation
                       ]
           }

areaTest :: Coord
areaTest = (11, 7)

areaProd :: Coord
areaProd = (101, 103)

data Robot = Robot { posX :: Int
                   , posY :: Int
                   , dX   :: Int
                   , dY   :: Int
                   } deriving (Show, Eq, Generic)

instance ToJSON Robot

-- Parser

robotArena :: Coord -> Parser (Grid [Robot])
robotArena (cols, rows) = do
  stuff <- many robot <* endOfInput
  pure Grid{trail = 0, ..}

robot :: Parser Robot
robot = do
  _ <- string "p="
  posX <- decimal
  _ <- char ','
  posY <- decimal
  _ <- string " v="
  dX <- signed decimal
  _ <- char ','
  dY <- signed decimal
  endOfLine
  pure Robot{..}

-- Math

part1Magic :: Grid [Robot] -> Int
part1Magic g = product $ groupSizes $ catMaybes $ map (toQuadrants g) gen100
  where gen100 = stuff $ iterate generation g !! 100
        groupSizes = map length . group . sort

moveRobot :: Grid a -> Robot -> Robot
moveRobot g Robot{..} = let newX = (posX + dX) `mod` (cols g)
                            newY = (posY + dY) `mod` (rows g)
                        in Robot{posX = newX, posY = newY, ..}

generation :: Grid [Robot] -> Grid [Robot]
generation g = g{stuff = map (moveRobot g) (stuff g)}

toQuadrants :: Grid a -> Robot -> Maybe (Bool, Bool)
toQuadrants g Robot{..} = let midX = cols g .>>. 1
                              midY = rows g .>>. 1
                          in if (posX == midX || posY == midY)
                             then Nothing
                             else Just (posX < midX, posY < midY)

-- |If the list is constructed with iteratively so that it depends
-- only of its last state and the data is cyclic, this returns one
-- cycle of it. Otherwise, endless list is returned.
uncycle :: Eq a => [a] -> [a]
uncycle [] = []
uncycle (x:xs) = x : takeWhile (x/=) xs

hasNoOverlaps :: Grid [Robot] -> Bool
hasNoOverlaps g = allUnique $ map toXY $ stuff g
  where toXY Robot{..} = (posX, posY)

allUnique :: Ord a => [a] -> Bool
allUnique list = f mempty list
  where f _ [] = True
        f s (x:xs) = if S.member x s
                     then False
                     else f (S.insert x s) xs

findTheNonOverlapping :: [Grid [Robot]] -> Int
findTheNonOverlapping gs = case filter (hasNoOverlaps . snd) (zip [0..] gs) of
  [(a, _)] -> a
  _        -> error "This dummy logic didn't work this time, sorry"
