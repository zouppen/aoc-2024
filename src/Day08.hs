{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day08 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Aeson (ToJSON)
import GHC.Generics
import Grid

import Day

task :: Day (Grid Antennas) Int
task = Day { parser = gridParser cell $ Antennas mempty
           , solvers = [("part1", ShowSolver $ length . tonttujoukko antinodesNoRepeat)
                       ,("part2", ShowSolver $ length . part2)
                       ]
           }

-- Types

type Coords = S.Set (Int, Int)

newtype Antennas = Antennas (M.Map Char Coords) deriving (Show, Generic)

instance ToJSON Antennas

type Producer a = (a, a) -> (a, a) -> [(a, a)]

-- Parsing

antennas :: Grid Antennas -> M.Map Char Coords
antennas g = let Antennas t = stuff g in t

cell :: Grid Antennas -> Parser Antennas
cell g = tower <|> blank
  where tower = (digit <|> letter_ascii) >>= pure . addTower g
        blank = char '.' >> pure (stuff g) -- Intact

addTower :: Grid Antennas -> Char -> Antennas
addTower g c = Antennas $ M.alter (Just . upd) c (antennas g)
  where upd Nothing    = S.singleton coord
        upd (Just old) = S.insert coord old
        coord = (trail g, rows g)

-- Work horse

-- |Repeat antinode until bounds reached to both directions
antinodesRepeat :: Grid a -> Producer Int
antinodesRepeat grid a b = side a b <> side b a
  where side x1 x2 = takeWhile (bounds grid) $ antinodeSideN x1 x2

-- |Repeat antinodes forever
antinodeSideN :: Num a => (a, a) -> (a, a) -> [(a, a)]
antinodeSideN a b = let c = antinodeSide a b
                    in c : antinodeSideN b c

-- |Give antinode on "right hand" side
antinodeSide :: Num a => (a, a) -> (a, a) -> (a, a)
antinodeSide (x1, y1) (x2, y2) = (2*x2-x1, 2*y2-y1)

-- |Push to the list a pair of antinodes
antinodesNoRepeat :: Num a => Producer a
antinodesNoRepeat a b = [antinodeSide a b, antinodeSide b a]

-- |Produces antinodes with given antinode producer function.
mapPairwise :: (a -> a -> [b]) -> [a] -> [b]
mapPairwise _ [] = []
mapPairwise f (a:xs) = concatMap (f a) xs <> mapPairwise f xs

-- |Produce antinode set from producer function and input set
antinodeSet :: Producer Int -> Grid a -> Coords -> Coords
antinodeSet producer grid setIn = S.fromList $
                                  filter (bounds grid) $
                                  (mapPairwise producer) $
                                  S.toList setIn

-- |Produces coordinates from all antinodes combined (excl. towers themselves)
tonttujoukko :: Producer Int -> Grid Antennas -> Coords
tonttujoukko f g = mconcat $ M.elems $ M.map (antinodeSet f g) $ antennas g

-- |Combine antinodes with antenna locations to get correct output.
part2 :: Grid Antennas -> Coords
part2 g = tonttujoukko (antinodesRepeat g) g <>
          (mconcat $ M.elems $ antennas g)
