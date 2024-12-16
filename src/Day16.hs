{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Day16 where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Set as S
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)

import Day
import Grid

task :: Day (Grid Arena) Int
task = Day { parser  = gridParser cell (Arena mempty mempty mempty) <* endOfInput
           , solvers = [ part 1 poroDijkstra
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

data Edge = Edge { fromNode :: Node
                 , toNode   :: Node
                 , cost     :: Int
                 } deriving (Show)

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
toEdges :: Grid Arena -> [Edge]
toEdges g = do
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
poroDijkstra g = case spLength start end graph of
                   Nothing -> error "No route found"
                   Just a  -> a
  where
    edges = toEdges g
    edgeSet = S.fromList [ node
                         | Edge{..} <- edges
                         , node     <- [fromNode, toNode]
                         ]
    edgeNum e = S.findIndex e edgeSet
    start = edgeNum Node { nodePos = head $ starts $ stuff $ g
                         , nodeDir = (1,0) -- East
                         }
    end = edgeNum End
    nodes = zip [0..] $ S.elems edgeSet
    edges' = [ (edgeNum fromNode, edgeNum toNode, cost)
             | Edge{..} <- edges
             ]
    graph = mkGraph nodes edges' :: Gr Node Int
