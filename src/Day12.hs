{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Day12 (task) where

import Data.Attoparsec.ByteString.Char8 hiding (takeWhile)
import Data.List (group)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Aeson (ToJSON)
import GHC.Generics
import Grid

import Day

task :: Day (Grid Plots) Int
task = Day { parser  = gridParser cell (Plots mempty)
           , solvers = [ part1 $ part1price . stuff
                       , part2 $ part2price
                       ]
           }

type Coord = (Int, Int)

newtype Plots = Plots (M.Map Char (S.Set Coord)) deriving (Show, Generic)

type Coords = S.Set Coord

instance ToJSON Plots

cell :: Grid Plots -> Parser Plots
cell g = do
  c <- letter_ascii
  let Plots oldMap = stuff g
      oldSet = M.findWithDefault mempty c oldMap
      newSet = S.insert (gridCoord g) oldSet
      newMap = M.insert c newSet oldMap
    in pure $ Plots newMap

formPeri :: Coords -> Int
formPeri s = sum $ map countEdges $ S.toList s
  where countEdges xy = length $ filter (flip S.notMember s) (sides xy)

sides :: (Num a, Num b) => (a, b) -> [(a, b)]
sides (x,y) = [ (x-1, y), (x, y-1)
              , (x+1, y), (x, y+1)
              ]

isles :: Coords -> [Coords]
isles s = case S.minView s of
  Just (xy, rest) -> let unreached = getTouching (S.singleton xy) rest
                     in S.difference s unreached : isles unreached
  Nothing -> []

getTouching :: Coords -> Coords -> Coords
getTouching wave unreached | null wave = unreached
                           | otherwise = getTouching newWave stillUnreached
  where potential = S.fromList $ S.toList wave >>= sides
        newWave = S.intersection unreached potential
        stillUnreached = S.difference unreached potential

part1price :: Plots -> Int
part1price (Plots plots) = sum $ [ length isle * formPeri isle
                                 | plot <- M.elems plots
                                 , isle <- isles plot
                                 ]

sillyWalks :: Grid a -> [((Int, Int), (Int, Int))]
sillyWalks Grid{..} = h <> v
  where h = [ ((x,y-1), (x, y))
            | y <- [0..cols] -- overshoot on both edges
            , x <- [0..rows] -- overshoot by one to cut streak
            ]
        v = [ ((x-1,y), (x, y))
            | x <- [0..rows] -- Same tricks as above but vertical
            , y <- [0..cols]
            ]

himmeli :: Grid a -> S.Set (Int, Int) -> Int
himmeli grid isle = length $ filter isEdge $ mergeSame $ map testSide $ sillyWalks grid
  where testSide (a, b) = (test a, test b)
        test = flip S.member isle
        isEdge (a,b) = a /= b
        mergeSame = map head . group

part2price :: Grid Plots -> Int
part2price grid = sum $ [ length isle * himmeli grid isle
                        | plot <- M.elems plots
                        , isle <- isles plot
                        ]
  where Plots plots = stuff grid
