{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Day10 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word

import Day
import Grid

task :: Day (Grid TrailMap) Int
task = Day { parser  = gridParser sparse $ TrailMap mempty mempty
           , solvers = [ part 1 $ walkAll (length . S.fromList)
                       , part 2 $ walkAll length
                       ]
           }

-- Types

data TrailMap = TrailMap { topo       :: M.Map Coord Word8
                         , trailheads :: [Coord]
                         } deriving (Show, Generic)

instance ToJSON TrailMap

-- Parsing

heightParser :: Parser Word8
heightParser = do
  a <- digit
  pure $ (fromIntegral $ fromEnum a) - 0x30

-- |Like 'cell' but allows dots in the examples as empty values.
sparse :: Grid TrailMap -> Parser TrailMap
sparse g = cell g <|> (char '.' >> pure (stuff g))

cell :: Grid TrailMap -> Parser TrailMap
cell Grid{..} = do
  h <- heightParser
  pure TrailMap{ topo = M.insert coord h $ topo stuff
               , trailheads = if h == 0 then coord : prevHeads else prevHeads
               }
  where coord = (trail, rows)
        prevHeads = trailheads stuff

-- Logic

walkAll :: Num a => ([Coord] -> a) -> Grid TrailMap -> a
walkAll reducer g = sum $ map (reducer . walk (topo $ stuff g) 0) (trailheads $ stuff g)

walk :: M.Map Coord Word8 -> Word8 -> Coord -> [Coord]
walk m level (x,y) =
  if (M.lookup (x,y) m == Just level)
  then if level == 9
       then [(x,y)] -- Summit reached
       else concatMap (walk m (level+1)) [ (x-1, y), (x, y-1)
                                         , (x+1, y), (x, y+1)
                                         ]
  else [] -- Out of bounds or too steep
