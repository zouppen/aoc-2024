{-# LANGUAGE RecordWildCards #-}
-- |Helper functions to make using of hgl route graph search less
-- complicated.
module AocTools.Routing ( Edge(..)
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
dijkstraLen edges start end = spLength (edgeNum start) (edgeNum end) graph
  where
    edgeSet = S.fromList [ node
                         | Edge{..} <- edges
                         , node     <- [fromNode, toNode]
                         ]
    edgeNum e = S.findIndex e edgeSet
    nodes = [(n, ()) | n <- [0..length edgeSet-1]]
    edges' = [ (edgeNum fromNode, edgeNum toNode, cost)
             | Edge{..} <- edges
             ]
    graph = mkGraph nodes edges' :: Gr () Int
