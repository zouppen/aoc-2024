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
import AocTools.Everyday (diamond, expectOne, liftMaybe)

task :: Day Arena Int
task = Day { parser  = stuff <$> gridParser cell (Arena mempty mempty mempty) <* endOfInput
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

toGraph :: Arena -> EasyGraph Coord Int
toGraph Arena{..} = easyGraph [ Edge{cost = 1, ..}
                              | fromNode <- S.toList ways
                              , toNode <- diamond 1 1 fromNode
                              , S.member toNode ways -- Target must exist
                              ]

wallhack :: Int -> Int -> Arena -> [(Coord, Coord, Int)]
wallhack duration saveLimit arena = do
  (cheatStart, startElapsed) <- dijkstraLens graph start
  -- Diamond-shaped search pattern around cheatStart point. Not
  -- looking direct neighbours (hollowness magic number 2)
  cheatEnd <- diamond duration 2 cheatStart
  endRemaining <- liftMaybe $ M.lookup cheatEnd revers
  let cheatDist = distance cheatStart cheatEnd
      totalDist = startElapsed + cheatDist + endRemaining
  -- Don't even show cheats which aren't good enough
  guard $ totalDist <= origShortest - saveLimit
  pure (cheatStart, cheatEnd, totalDist)
  where start = expectOne "start point" $ starts arena
        end = expectOne "end point" $ ends arena
        distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
        graph = toGraph arena
        -- In reverse search we save time by not reversing the edges
        -- since it's already bidirectional.
        revers = M.fromList $ dijkstraLens graph end
        -- Distance without cheating
        origShortest = revers M.! start
