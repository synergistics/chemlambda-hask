module Chemlambda.Core.Connection 
  where
  
import Data.List
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))

import Chemlambda.Core.Atom


data PortType = LO | LI | RO | RI | MO | MI
  deriving ( Eq, Show )

data Direction = I | O 
  deriving ( Eq, Ord, Show )

type PortRef = (Int,PortType)
  
data Node a = Node a 
  deriving ( Eq, Show )

data Edge a = Edge a a 
  deriving ( Eq, Show )

data Graph = Graph 
  { graphNodes :: IntMap (Node Atom) 
  , graphEdges :: IntMap (Edge PortRef) } 
  deriving ( Eq, Show )


isOut :: PortType -> Bool
isOut = flip elem [LO, RO, MO]

isIn :: PortType -> Bool
isIn = flip elem [LI, RI, MI]

direction :: PortType -> Direction
direction p 
  | isOut p = O
  | isIn  p = I


getNode :: Graph -> Int -> (Node Atom) 
getNode = (!) . graphNodes 

getEdge :: Graph -> Int -> (Edge PortRef) 
getEdge = (!) . graphEdges 

portGroups :: [( n, [PortRef] )] -> [[PortRef]]
portGroups entries = 
  let
    portEntries = sortBy (\p q -> compare (fst p) (fst q)) $ concatMap snd entries
    groups      = groupBy (\ i j -> fst i == fst j) portEntries
  in
   groups


addFrees :: [( Node Atom, [PortRef] )] -> [( Node Atom, [PortRef] )]
addFrees entries = 
  let
    portSingletons
      = map head 
      $ filter ((== 1) . length) 
      $ portGroups entries 

    frees = map 
      (\(i,pt) -> 
        case direction pt of 
          I -> ( Node FRIN,  [(i, MO)] )
          O -> ( Node FROUT, [(i, MI)] ))
      portSingletons
  in
    frees ++ entries

samePort :: PortRef -> PortRef -> Bool
samePort i j = fst i == fst j

toGraph :: [( Node Atom, [PortRef] )] -> Graph
toGraph entries =
  let
    entries' = addFrees entries

    indexedEntries = zipWith (\(n, es) i -> (i, es)) entries' [0..]
    indicesSharingEdge 
      = groupBy (\(_, pe1) (_, pe2) -> samePort pe1 pe2)
      $ sortBy (\ (_, (a,p)) (_, (b,q)) -> compare (a, direction p) (b, direction q))
      $ concatMap (\(i, ports) -> map (\p -> (i, p)) ports) indexedEntries
    nodes 
      = IntMap.fromList
      $ zip [0..]
      $ map fst entries'

    edges 
      = IntMap.fromList 
      $ map (\[ (i, (pi, pt1)), 
                (j, (_ , pt2))  ] -> (pi, Edge (i,pt1) (j,pt2))) indicesSharingEdge
  in 
    Graph nodes edges 


matchEdge :: Edge (Atom,PortType) -> Graph -> [(Int,Edge PortRef)]
matchEdge (Edge (a,ptA) (b,ptB)) graph = 
  filter 
    (\(_, (Edge (i,ptI) (j,ptJ))) -> 
      ptA == ptI && ptB == ptJ && Node a == getNode graph i && Node b == getNode graph j)
    (IntMap.assocs $ graphEdges graph)

test :: [(Node Atom, [PortRef])]
test = 
  [ (Node A, [ (3,LI), (4,RI), (5,MO) ]) 
  , (Node L, [ (1,MI), (1,LO), (3,RO) ]) ]
