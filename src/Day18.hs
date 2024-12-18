{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Day18 where

import Control.Applicative
import Control.Monad (guard)
import qualified Data.Array as A
import Data.Attoparsec.ByteString.Char8
import Data.Maybe (isNothing)
import qualified Data.Set as S

import Day
import AocTools.Everyday (bsearch)
import AocTools.Grid
import AocTools.Routing

task :: Day (Grid Coords) Int
task = Day { parser  = memParse (71, 71)
           , solvers = [ part 1 $ routeLenPt1 1024
                       , partStr 2 $ showCoord . lastMoment 1025
                       ]
           }

newtype Coords = Coords (A.Array Int Coord) deriving (Show)

instance ToJSON Coords where
  toJSON (Coords a) = toJSON $ A.elems a

-- Parsing

memParse :: Coord -> Parser (Grid Coords)
memParse (w, h) = do
  cells <- many cell
  endOfInput
  pure $ Grid{ stuff = Coords $ A.listArray (0, length cells-1) cells
             , cols = w, rows = h, trail = 0 }

cell :: Parser Coord
cell = do
  x <- decimal
  _ <- char ','
  y <- decimal
  endOfLine
  pure (x,y)

-- Logic part

toEdges :: Grid Coords -> Int -> [Edge Coord]
toEdges Grid{..} pivot = do
  y <- [0..rows-1]
  x <- [0..cols-1]
  let me = (x,y)
  guard $ isEmpty me
  dir <- unitCoords
  pure $ Edge{ fromNode = me
             , toNode   = addCoord me dir
             , cost     = 1
             }
  where Coords arr = stuff
        subArr = A.ixmap (0, pivot-1) id arr
        isEmpty x = S.notMember x $ S.fromList $ A.elems subArr

routeLen :: Grid Coords -> Int -> Maybe Int
routeLen g n = dijkstraLen (toEdges g n) (0,0) (cols g-1, rows g-1)

-- Part 1

routeLenPt1 :: Int -> Grid Coords -> Int
routeLenPt1 n g = case routeLen g n of
  Nothing -> error "Route not found"
  Just x -> x

-- Part 2

lastMoment :: Int -> Grid Coords -> Coord
lastMoment start g = arr A.! (item-1)
  where Coords arr = stuff g
        (_, upper) = A.bounds arr
        proj = isNothing . routeLen g
        item = bsearch proj start (upper+1)

showCoord :: Coord -> String
showCoord (x,y) = show x <> "," <> show y
