{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Language.Cimple.Graph
  ( Graph
  , fromEdges
  , edges
  ) where

import qualified Data.Graph as G

data Graph node key = Graph
    { graph          :: G.Graph
    , nodeFromVertex :: G.Vertex -> (node, key, [key])
    , vertexFromKey  :: key -> Maybe G.Vertex
    }

fromEdges :: Ord key => [(node, key, [key])] -> Graph node key
fromEdges es = Graph{..}
  where
    (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges es

edges :: Graph node key -> [(key, key)]
edges Graph{..} = map resolve . G.edges $ graph
  where
    resolve (from, to) =
      let
          (_, from', _) = nodeFromVertex from
          (_, to', _) = nodeFromVertex to
      in
      (from', to')
