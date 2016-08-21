{-# LANGUAGE GADTs #-}
module Chemlambda.Core.Pattern
  where

import qualified Data.IntMap as IntMap
import qualified Data.Maybe as Maybe
import Chemlambda.Core.Atom
import Chemlambda.Core.Connection


data Pattern a where
  AnyP      :: Pattern a
  PortP     :: PortType -> Pattern PortType
  OutPortP  :: Pattern PortType
  InPortP   :: Pattern PortType
  AtomP     :: Atom -> Pattern Atom
  NodeRefP  :: Pattern Atom -> Pattern PortType -> Pattern (NodeRef Atom)
  EdgeP     :: Pattern (NodeRef Atom) ->  Pattern (NodeRef Atom) -> Pattern (Edge (NodeRef Atom))

type EdgePattern = Pattern (Edge (NodeRef Atom)) 

runPattern :: Pattern a -> a -> Bool
runPattern AnyP           = const True
runPattern (PortP p)      = (==) p
runPattern OutPortP       = (== O) . direction
runPattern InPortP        = (== I) . direction
runPattern (AtomP a)      = (==) a
runPattern (NodeRefP a p) = \(NR a' p') -> runPattern a a' && runPattern p p'
runPattern (EdgeP a b)    = \(Edge a' b') -> runPattern a a' && runPattern b b'

matchEdge :: EdgePattern -> Graph -> [(Int, Edge (NodeRef Int))]
matchEdge ep g
  = filter (\(i,e) -> runPattern ep (edgeWithAtoms e g))
  $ IntMap.assocs
  $ graphEdges g


betaPattern :: EdgePattern
betaPattern = EdgeP (NodeRefP (AtomP A) (PortP LI)) (NodeRefP (AtomP L) (PortP RO))

betaMove :: Graph -> Graph
betaMove graph =
  let
    (i, (Edge (NR aRef aP) (NR lRef lP))) = head $ matchEdge betaPattern graph
    (aNode, aERs) = getNode graph aRef 
    (lNode, lERs) = getNode graph lRef 

    Just (iA, Edge arrow1MI xO) = edgeAtPort (NR lRef MI) graph
    Just (iB, Edge yI lLO)      = edgeAtPort (NR lRef LO) graph
    Just (iC, Edge aRI zO)      = edgeAtPort (NR aRef RI) graph
    Just (iD, Edge wI arrow2MO) = edgeAtPort (NR aRef MO) graph

    arrow1MO = NR lRef MO
    arrow2MI = NR aRef MI

    arrow1ERs = foldr 
      (\er ers ->
        case ePT er of
          RO -> ers
          LO -> ER (eRef er) MO : ers
          MI -> Maybe.fromJust (edgeRefAtPort aERs RI) : ers)
        []
        lERs

    arrow2ERs = foldr 
      (\er ers ->
        case ePT er of
          LI -> ers
          RI -> ER (eRef er) MI : ers
          MO -> Maybe.fromJust (edgeRefAtPort lERs LO) : ers)
        []
        aERs
    
    edgeA = (iA, Edge arrow1MI xO)
    edgeB = (iB, Edge yI arrow2MO)
    edgeC = (iC, Edge arrow2MI zO) 
    edgeD = (iD, Edge wI arrow1MO)
    newEdges = IntMap.delete i $ foldl
      (\im (i,e) -> IntMap.insert i e im)
      (graphEdges graph) 
      [edgeA,edgeB,edgeC,edgeD]

    nodes = [(lRef, (Node ARROW, arrow1ERs)), (aRef, (Node ARROW, arrow2ERs))]
    newNodes = foldl
      (\im (i,n) -> IntMap.insert i n im)
      (graphNodes graph)
      nodes

  in Graph newNodes newEdges 
