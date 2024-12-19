{-# LANGUAGE RecordWildCards #-}
-- |Helper functions to make using of hgl route graph search less
-- complicated.
module AocTools.Routing
  ( Edge(..)
  , dijkstraLen
  ) where

import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)
import qualified Data.Set as S

data Edge a = Edge { fromNode :: a
                   , toNode   :: a
                   , cost     :: Int
                   } deriving (Show)

-- |Finds a shortest path. Simplification of the generic graph
-- library. Abstracts the the node numbering and stuff. Just give
-- edges and nodes are created automatically.
dijkstraLen :: Ord a => [Edge a] -> a -> a -> Maybe Int
dijkstraLen edges start end = spLength (nodeNum start) (nodeNum end) graph
  where
    nodeSet = S.fromList [ node
                         | Edge{..} <- edges
                         , node     <- [fromNode, toNode]
                         ]
    nodeNum e = S.findIndex e nodeSet
    nodes = [(n, ()) | n <- [0..length nodeSet-1]]
    edges' = [ (nodeNum fromNode, nodeNum toNode, cost)
             | Edge{..} <- edges
             ]
    graph = mkGraph nodes edges' :: Gr () Int
