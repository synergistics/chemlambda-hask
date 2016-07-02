module Chemlambda.Graph where

import Data.List 
import qualified Util.Graph as G
import Chemlambda.Node
import Util.Pattern


-- Maybe make these polymorphic on the edge type
type Graph   = G.Graph Node Int
type Context = G.Context Node Int
type View    = G.View Node Int


-- Returns the element of a graph that has the incoming edge of e
withInEdge :: Int -> Graph -> Maybe View
withInEdge e g = (G.:& g) <$> find (\c -> elem e (G.inEdges c)) g

withOutEdge :: Int -> Graph -> Maybe View
withOutEdge e g = (G.:& g) <$> find (\c -> elem e (G.outEdges c)) g


