{-# LANGUAGE RecordWildCards #-}
-- |Helper functions to make using of hgl route graph search less
-- complicated.
module AocTools.Routing
  ( Edge(..)
  , EasyGraph(..)
  , dijkstraLen
  , dijkstraLens
  , labelByNode
  , nodeByLabel
  , easyGraph
  , revGraph
  ) where

import Data.Graph.Inductive.Graph ( Node, mkGraph, lab, labNodes
                                  , labEdges, unLPath)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spTree, spLength)
import qualified Data.Set as S

data Edge a b = Edge { fromNode :: a
                     , toNode   :: a
                     , cost     :: b
                     } deriving (Show)

data EasyGraph a b = EasyGraph { graph    :: Gr a b
                               , nodeSet  :: S.Set a
                               } deriving (Show)

nodeByLabel :: Ord a => EasyGraph a b -> a -> Maybe Node
nodeByLabel eg a = S.lookupIndex a $ nodeSet eg

labelByNode :: EasyGraph a b -> Node -> Maybe a
labelByNode eg = lab $ graph eg

-- |Finds shortest path length.
dijkstraLen :: (Ord a, Real b) => EasyGraph a b -> a -> a -> Maybe b
dijkstraLen eg start end = spLength (n "Start" start) (n "End" end) (graph eg)
  where n name node = case nodeByLabel eg node of
          Just a -> a
          Nothing -> error $ name <>" node is not part of the graph"

-- |Finds shortest paths.
dijkstraLens :: (Ord a, Real b) => EasyGraph a b -> a -> [(a, b)]
dijkstraLens eg start = clean $ spTree start' $ graph eg
  where start'  = case nodeByLabel eg start of
          Just a -> a
          Nothing -> error $ "Start node is not part of the graph"
        toLab node = case labelByNode eg node of
          Just a -> a
          Nothing -> error "Tampered graph"
        clean = map $ toLabPair . takeTarget
        takeTarget = head . unLPath
        toLabPair (a, b) = (toLab a, b)

-- |Generates a graph with support to bidirectional node id
-- lookups. Abstracts the the node numbering and stuff. Just give
-- edges and nodes are created and numbered automatically. Don't alter
-- the returned nodeSet because that messes node number lookups.
easyGraph :: Ord a => [Edge a b] -> EasyGraph a b
easyGraph edges = EasyGraph{..}
  where
    nodeSet = S.fromList [ node
                         | Edge{..} <- edges
                         , node     <- [fromNode, toNode]
                         ]
    nodeNum e = S.findIndex e nodeSet
    nodes = zip [0..] $ S.elems nodeSet
    graph = mkGraph nodes [ (nodeNum fromNode, nodeNum toNode, cost)
                          | Edge{..} <- edges
                          ]

-- |Reverses edges in a graph.
revGraph :: EasyGraph a b -> EasyGraph a b
revGraph eg = eg{ graph = newGraph }
  where rev (src, dst, cost) = (dst, src, cost)
        g = graph eg
        newGraph = mkGraph (labNodes g) (map rev $ labEdges g)
