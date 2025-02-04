{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Day16 where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Day
import AocTools.Grid
import AocTools.Routing
import AocTools.Everyday (minFold)

task :: Day (Grid Arena) Int
task = Day { parser  = gridParser cell (Arena mempty mempty mempty) <* endOfInput
           , solvers = [ part 1 poroDijkstra
                       , part 2 onShortestPaths
                       ]
           }

data Arena = Arena { ways   :: S.Set Coord
                   , starts :: [Coord]
                   , ends   :: [Coord]
                   } deriving (Show, Generic)

instance ToJSON Arena

data Node = End
          | Node { nodePos :: Coord
                 , nodeDir :: Coord
                 } deriving (Show, Eq, Ord)

-- Parser

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

-- Computer science part

-- |I heard you like list monad so I put list in your list so you can
-- iterate while you iterate.
toGraph :: Grid Arena -> EasyGraph Node Int
toGraph g = easyGraph $ do
  -- For each waypoint
  me <- S.toList $ ways $ stuff g
  -- To every direction
  dir <- unitCoords
  let neighs = do
        -- List of neighbors, if any, walking straigth
        let neigh = addCoord me dir
            node a = Node{nodePos = a, nodeDir = dir}
        guard $ S.member neigh $ ways $ stuff g
        pure $ Edge{ fromNode = node me
                   , toNode   = node neigh
                   , cost     = 1
                   }
      turns = do
        -- List of valid turns without movement
        let node a = Node{nodePos = me, nodeDir = a}
        turn <- [turnLeft, turnRight]
        pure $ Edge{ fromNode = node dir
                   , toNode   = node (turn dir)
                   , cost     = 1000
                   }

  -- The goal can be reached from every direction. Otherwise output normal
  -- movements and rotations.
  if me `elem` ends (stuff g)
    then pure $ Edge{ fromNode = Node{ nodePos = me, nodeDir = dir}
                    , toNode   = End
                    , cost     = 0
                    }
    else neighs <> turns

poroDijkstra :: Grid Arena -> Int
poroDijkstra g = case dijkstraLen (toGraph g) (startNode g) End of
                   Nothing -> error "No route found"
                   Just a  -> a

startNode :: Grid Arena -> Node
startNode g = Node { nodePos = head $ starts $ stuff $ g
                   , nodeDir = (1,0) -- East
                   }

-- |Run dijkstra from start and from end (with edges reversed) and get
-- an intersection of those two graphs, returning all unique
-- coordinates which are part of some shortest path.
onShortestPaths :: Grid Arena -> Int
onShortestPaths g = length $ spotCoords
  where graph = toGraph g
        normal = M.fromList $ dijkstraLens graph (startNode g)
        revers = M.fromList $ dijkstraLens (revGraph graph) End
        walkable = M.intersectionWith (+) normal revers
        (_, spotNodes) = M.foldrWithKey (minFold compare) (maxBound, []) walkable
        spotCoords = S.fromList $ map nodePos $ filter (End/=) spotNodes
