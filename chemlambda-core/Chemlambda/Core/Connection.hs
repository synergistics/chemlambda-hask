{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Chemlambda.Core.Connection where
  
import Data.List
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!))

import Chemlambda.Core.Atom


data PortType = LO | LI | RO | RI | MO | MI deriving ( Eq, Show )

data Direction = O | I deriving ( Eq, Show )

isOut :: PortType -> Bool
isOut = flip elem [LO, RO, MO]

isIn :: PortType -> Bool
isIn = flip elem [LI, RI, MI]

direction :: PortType -> Direction
direction p 
  | isOut p = O
  | isIn  p = I


data Node a = Node a deriving ( Eq, Show )

data Edge where
  Edge :: (Show a, Num a) => a -> (a, PortType) -> (a, PortType) -> Edge
deriving instance Show Edge 

data Graph = Graph 
  { graphNodes :: Vector (Node Atom) 
  , graphEdges :: Vector Edge } 
  deriving ( Show )
-- getNode :: Graph -> Int -> (Node Atom) 
getNode = (!) . graphNodes 

-- getEdge :: Graph -> Int -> Edge 
getEdge = (!) . graphEdges 

edgeGroups :: (Num a, Eq a) => [( n, [(a, PortType)] )] -> [[(a, PortType)]]
edgeGroups nodeEntries = 
  let
    edgeEntries = concatMap snd nodeEntries
    groups      = groupBy (\i j -> fst i == fst j) edgeEntries
  in
   groups


addFrees :: (Num a, Eq a) => [( Node Atom, [(a, PortType)] )] -> [( Node Atom, [(a, PortType)] )]
addFrees nodeEntries = 
  let
    portSingletons
      = map head 
      $ filter ((== 1) . length) 
      $ edgeGroups nodeEntries 

    frees = map 
      (\(i,pt) -> 
        case direction pt of 
          I -> ( Node FRIN,  [(i, MO)] )
          O -> ( Node FROUT, [(i, MI)] ))
      portSingletons
  in
    frees ++ nodeEntries

samePort :: Eq a => (a, PortType) -> (a, PortType) -> Bool
samePort i j = fst i == fst j

toGraph :: (Num a, Eq a, Show a, Enum a) => [( Node Atom, [(a, PortType)] )] -> Graph -- Edge GADT makes constraints list long. 
toGraph nodeEntries =
  let
    nodeEntries'   = addFrees nodeEntries

    indexedEntries = zipWith (\(n, es) i -> (i, es)) nodeEntries' [0..]
    indicesSharingEdge 
      = groupBy (\(node1, pe1) (node2, pe2) -> samePort pe1 pe2)
      $ concatMap (\(i, ports) -> map (\p -> (i, p)) ports) indexedEntries
    
    nodes = Vector.fromList $ map fst nodeEntries'

    edges 
      = Vector.fromList 
      $ map (\[(i, (pi, pt1)), (j, (_, pt2))] -> Edge pi (i,pt1) (j,pt2)) indicesSharingEdge
  in 
    Graph nodes edges 

test = 
  [ (Node L, [(1,MI), (1,LO), (3,RO)])
  , (Node A, [(3,LI)]) ]
