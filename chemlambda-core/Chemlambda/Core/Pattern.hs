module Chemlambda.Core.Pattern
  where

import qualified Data.IntMap as IntMap
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IntMap (IntMap, (!))
import Data.Maybe (Maybe)
import Data.Map (Map)

import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.AdjList


type PatternResult = Map (Atom,PortType) (Int, Node Atom Int)


getMatchingNodes (a,ptA) (b,ptB) graph
  = IntMap.toList
  $ IntMap.filterWithKey
    (\i node ->
      if atom node == a
      then let
          atPortA
             =  getNode graph
            <$> nRef
            <$> refAtPort node ptA  

          atPortB
             =  getNode graph
            <$> nRef
            <$> (atPortA >>= flip refAtPort ptB)
        in (atom <$> atPortA) == Just b && atPortB == Just node  
      else False)
      (unGraph graph)
    
runPattern
  :: (Atom,PortType) -- AtomA
  -> (Atom,PortType) -- AtomB
  -> [PortType]      -- Select nodes at these ports of AtomA
  -> [PortType]      -- Select nodes at these ports of AtomB
  -> Graph Atom      -- Initial graph --
  -> [PatternResult] -- List of "subgraphs" that match the pattern
runPattern (a,ptA) (b,ptB) portsA portsB graph =
  let
    nodeAssocs    = IntMap.assocs $ unGraph graph
    matchingNodes = getMatchingNodes (a,ptA) (b,ptB) graph
    
    mkKeys atom pts = map ((,) atom) pts

    results =
      foldr
        (\(i, nodeA@(Node a nrs)) acc ->
          let
            (Just nodeB) = nodeAtPort nodeA ptA graph

            nodeRefs
              = ((++)
                  (map (\port -> 
                    let
                      nodeRef = refAtPort nodeA port
                      ref     = nRef <$> nodeRef
                    in fmap (\r -> (r, getNode graph r)) ref)
                    portsA)
                  (map (\port ->
                    let
                      nodeRef = refAtPort nodeB port
                      ref     = nRef <$> nodeRef
                    in fmap (\r -> (r, getNode graph r)) ref)
                    portsB))

            keys = mkKeys a portsA ++ mkKeys b portsB
          -- in if List.any Maybe.isNothing nodeRefs
          --   then acc 
          in (Map.fromList $ zip keys (map Maybe.fromJust nodeRefs)) : acc)
        []
        matchingNodes

  in results


betaPattern :: Graph Atom -> [PatternResult]
betaPattern = runPattern (L,RO) (A,LI) [MI,LO,RO] [LI,RI,MO]

-- betaMove :: PatternResult -> Maybe (Graph Atom) 
betaMove nodes =
  let
    lam   = Map.lookup (A,LI) nodes
    app   = Map.lookup (L,RO) nodes
    miLam = Map.lookup (L,MI) nodes
    loLam = Map.lookup (L,LO) nodes
    riApp = Map.lookup (A,RI) nodes
    moApp = Map.lookup (A,MO) nodes
   
  in do
    (iL,   lamNode) <- lam
    (iA,   appNode) <- app
    (iMiL, miLamNode) <- miLam
    (iLoL, loLamNode) <- loLam
    (iRiA, riAppNode) <- riApp
    (iMoA, moAppNode) <- moApp

    refAi <- refAtPort lamNode MI
    let refAo = head $ refsWithVal miLamNode iL

    let refBi = head $ refsWithVal loLamNode iL
    refBo <- adjustRef id (const MO) <$> refAtPort lamNode LO

    refDi <- adjustRef id (const MI) <$> refAtPort appNode RI
    let refDo = head $ refsWithVal riAppNode iA

    refEo <- refAtPort appNode MO
    let refEi = head $ refsWithVal moAppNode iA

    let arrNode1 = (iL, Node ARROW [refAi, refEo])
    let arrNode2 = (iA, Node ARROW [refDi, refBo])

    let rawNodes = Map.elems nodes
    let withRemoved = filter (\n -> not $ elem n [(iL,lamNode),(iA,appNode)]) $ rawNodes
    
    let newLoL = (iLoL, adjustRefInNode refBi (NR iA (nPT refBi)) loLamNode)
    let newMoA = (iMoA, adjustRefInNode refEi (NR iL (nPT refEi)) moAppNode)

    let newNodes' = withRemoved ++ [arrNode1, arrNode2, newLoL, newMoA]

    return $ Graph $ IntMap.fromList $ newNodes' 


