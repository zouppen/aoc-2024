{-# LANGUAGE RecordWildCards #-}
-- |Helper functions to make using of hgl route graph search less
-- complicated.
module AocTools.Routing
  ( Edge(..)
  , EasyGraph(..)
  , dijkstraLen
  , labelByNode
  , nodeByLabel
  , easyGraph
  ) where

import Data.Graph.Inductive.Graph (Node, mkGraph, lab)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)
import qualified Data.Set as S

data Edge a b = Edge { fromNode :: a
                     , toNode   :: a
                     , cost     :: b
                     } deriving (Show)

data EasyGraph a b = EasyGraph { graph   :: Gr a b
                               , nodeSet :: S.Set a
                               } deriving (Show)

nodeByLabel :: Ord a => EasyGraph a b -> a -> Maybe Node
nodeByLabel eg a = S.lookupIndex a $ nodeSet eg

labelByNode :: EasyGraph a b -> Node -> Maybe a
labelByNode eg = lab $ graph eg

-- |Finds a shortest path. Uses 'easyGraph' internally, see the docs.
dijkstraLen :: (Ord a, Real b) => [Edge a b] -> a -> a -> Maybe b
dijkstraLen edges start end = spLength (n "Start" start) (n "End" end) graph
  where eg@EasyGraph{..} = easyGraph edges
        n name node = case nodeByLabel eg node of
          Just a -> a
          Nothing -> error $ name <>" node is not part of the graph"

-- |Generates a graph with support to bidirectional node id
-- lookups. Abstracts the the node numbering and stuff. Just give
-- edges and nodes are created and numbered automatically.
easyGraph :: Ord a => [Edge a b] -> EasyGraph a b
easyGraph edges = EasyGraph{..}
  where
    nodeSet = S.fromList [ node
                         | Edge{..} <- edges
                         , node     <- [fromNode, toNode]
                         ]
    nodeNum e = S.findIndex e nodeSet
    nodes = zip [0..] $ S.elems nodeSet
    edges' = [ (nodeNum fromNode, nodeNum toNode, cost)
             | Edge{..} <- edges
             ]
    graph = mkGraph nodes edges'
