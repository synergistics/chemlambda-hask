module Chemlambda.Core.Pattern
  where

import qualified Data.IntMap as IntMap
import qualified Data.Maybe as Maybe
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.Graph


betaPattern :: Graph Atom -> [Graph Atom] --[(Int, Node Atom Int)]
betaPattern graph =
  let
    nodeAssocs = IntMap.assocs $ unGraph graph
    matchingLs =
      filter
        (\(i, node@(Node n ps)) ->
          let
            atRo 
              = getNode graph
              $ nRef                 
              $ Maybe.fromJust
              $ refAtPort node RO    
            
            atLi
              = getNode graph
              $ nRef
              $ Maybe.fromJust
              $ refAtPort atRo LI

          in n == L && atom atRo == A && atLi == node)
        nodeAssocs
    
    subGraphs =
      map
        (\(i, lamNode@(Node n ps)) ->
          let
            (Just appNode) = Maybe.fromJust $ nodeAtPort lamNode RO graph
            nodeRefs = i : map (nRef . Maybe.fromJust)
              [ refAtPort lamNode MI
              , refAtPort lamNode LO
              , refAtPort lamNode RO
              , refAtPort appNode RI
              , refAtPort appNode MO
              ]
          in subGraph nodeRefs graph)
        matchingLs

  in subGraphs
