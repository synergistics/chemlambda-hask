module Chemlambda.Core.Pattern
  where

import qualified Data.IntMap as IntMap
import Data.IntMap ((!))
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.Graph


mkPattern
  :: (Atom,PortType) -- NR1
  -> (Atom,PortType) -- NR2
  -> [PortType]      -- Select nodes at these ports of NR1
  -> [PortType]      -- Select nodes at these ports of NR2
  -> Graph Atom      -- Initial graph
  -> [Graph Atom]    -- List of subgraphs that match the pattern
mkPattern (nrA,ptA) (nrB,ptB) portsA portsB graph =
  let
    nodeAssocs = IntMap.assocs $ unGraph graph

    matchingNRA =
      filter
        (\(i, node) ->
          if atom node == nrA
          then let
              atPortA
                =  getNode graph
               <$> nRef
               <$> refAtPort node ptA  

              atPortB
                =  getNode graph
               <$> nRef
               <$> (atPortA >>= flip refAtPort ptB)
            in (atom <$> atPortA) == Just nrB && atPortB == Just node  
          else False)
        nodeAssocs
    
    subGraphs =
      map
        (\(i, nodeA@(Node a nrs)) ->

          let
            (Just nodeB) = nodeAtPort nodeA ptA graph
            nodeRefs     = Just i : map (nRef <$>)
              ((++)
                (map (refAtPort nodeA) portsA)
                (map (refAtPort nodeB) portsB))
          in 
            if List.any Maybe.isNothing nodeRefs
            then Graph $ IntMap.empty 
            else subGraph (map Maybe.fromJust nodeRefs) graph)

        matchingNRA

  in subGraphs


betaPattern :: Graph Atom -> [Graph Atom]
betaPattern = mkPattern (L,RO) (A,LI) [MI,LO,RI] [RI,MO]
