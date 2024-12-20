{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Day20 where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Day
import AocTools.Grid
import AocTools.Routing
import AocTools.Everyday (diamond)

task :: Day (Grid Arena) Int
task = Day { parser  = gridParser cell (Arena mempty mempty mempty) <* endOfInput
           , solvers = [ part 1 $ length . wallhack 2 100
                       , part 2 $ length . wallhack 20 100
                       ]
           }

data Arena = Arena { ways   :: S.Set Coord
                   , starts :: [Coord]
                   , ends   :: [Coord]
                   } deriving (Show, Generic)

instance ToJSON Arena

-- Parsing

cell :: Grid Arena -> Parser Arena
cell g = anyChar >>= \c -> case c of
  '#' -> pure old -- Walls are not stored
  '.' -> pure way
  'S' -> pure way{starts = pos:starts way}
  'E' -> pure way{ends   = pos:ends way}
  _   -> empty
  where old = stuff g
        pos = gridCoord g
        way = old{ways = S.insert pos $ ways old}

-- Logic part

toGraph :: Grid Arena -> EasyGraph Coord Int
toGraph Grid{..} = easyGraph $ do
  fromNode <- S.toList $ ways stuff
  toNode <- diamond 1 1 fromNode
  guard $ S.member toNode $ ways stuff -- Target must exist
  pure $ Edge{cost = 1, ..}

wallhack :: Int -> Int -> Grid Arena -> [(Coord, Coord, Int)]
wallhack duration saveLimit g = do
  (cheatStart, startElapsed) <- dijkstraLens graph start
  -- Diamond-shaped search pattern around cheatStart point
  cheatEnd <- diamond duration 1 cheatStart
  endRemaining <- maybe empty pure $ M.lookup cheatEnd revers
  let cheatDist = distance cheatStart cheatEnd
      totalDist = startElapsed + cheatDist + endRemaining
  -- Don't even show cheats which aren't good enough
  guard $ totalDist <= origShortest - saveLimit
  pure (cheatStart, cheatEnd, totalDist)
  where [start] = starts $ stuff g
        [end] = ends $ stuff g
        graph = toGraph g
        -- In reverse search we save time by not reversing the edges
        -- since it's already bidirectional.
        revers = M.fromList $ dijkstraLens graph end
        -- Distance without cheating
        origShortest = revers M.! start
        distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
