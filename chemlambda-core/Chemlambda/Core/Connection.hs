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

data NodeRef = NR { nRef :: Int, nPT :: PortType } deriving ( Eq )
data EdgeRef = ER { eRef :: Int, ePT :: PortType } deriving ( Eq )

instance Show NodeRef where
  show (NR i pt) = show (i,pt)
 
instance Show EdgeRef where
  show (ER i pt) = show (i,pt)

data Node a = Node { unNode :: a }
  deriving ( Eq )

instance Show a => Show (Node a) where
  show (Node a) = "Node " ++ show a

data Edge a = Edge a a
  deriving ( Eq, Show )

data Graph = Graph
  { graphNodes :: IntMap (Node Atom,[EdgeRef])
  , graphEdges :: IntMap (Edge NodeRef) }
  deriving ( Eq, Show )


isOut :: PortType -> Bool
isOut = flip elem [LO, RO, MO]

isIn :: PortType -> Bool
isIn = flip elem [LI, RI, MI]

direction :: PortType -> Direction
direction p
  | isOut p = O
  | isIn  p = I


getNode :: Graph -> Int -> (Node Atom,[EdgeRef]) 
getNode = (!) . graphNodes

getEdge :: Graph -> Int -> (Edge NodeRef)
getEdge = (!) . graphEdges

portGroups :: [(n,[NodeRef])] -> [[NodeRef]]
portGroups entries =
  let
    portEntries = sortBy  (\ p q -> compare (nRef p) (nRef q)) $ concatMap snd entries
    groups      = groupBy (\ p q -> nRef p == nRef q) portEntries
  in
   groups


addFrees :: [( Node Atom, [NodeRef] )] -> [( Node Atom, [NodeRef] )]
addFrees entries =
  let
    portSingletons
      = map head
      $ filter ((== 1) . length)
      $ portGroups entries

    frees = map
      (\(NR i pt) ->
        case direction pt of
          I -> ( Node FRIN,  [NR i MO] )
          O -> ( Node FROUT, [NR i MI] ))
      portSingletons
  in
    frees ++ entries

-- samePort :: (Int,PortType) -> (Int,PortType) -> Bool
-- samePort i j = fst i == fst j

toGraph :: [( Node Atom, [NodeRef] )] -> Graph
toGraph entries =
  let
    entries' = addFrees entries

    indexedEntries = zipWith (\(_,nrs) i -> (i,nrs)) entries' [0..]
    indicesSharingEdge
      = groupBy (\(_,(NR i pe1)) (_,(NR j pe2)) -> i == j)
      $ sortBy  (\(_,(NR a p)) (_,(NR b q)) -> compare (a, direction p) (b, direction q))
      $ concatMap (\(i,nrs) -> map ((,) i) nrs) indexedEntries

    nodes
      = IntMap.fromList
      $ zip [0..] $ map (\(n,nrs) -> (n, map (\(NR i pt) -> ER i pt) nrs)) entries'

    edges
      = IntMap.fromList
      $ map (\[ (i, (NR pi pt1))
              , (j, (NR _  pt2)) ] -> (pi, Edge (NR i pt1) (NR j pt2))) indicesSharingEdge
    
  in
    Graph nodes edges

-- -- toAtomPort :: NodeRef -> Graph -> (Atom,PortType)
-- -- toAtomPort (i,pt) g = (unNode $ getNode g i, pt)

-- -- toAtomEdge :: (Edge NodeRef) -> Graph -> Edge (Atom,PortType)
-- -- toAtomEdge (Edge apA apB) g = Edge (toAtomPort apA g) (toAtomPort apB g)

test :: [(Node Atom, [NodeRef])]
test =
  [ (Node A, [ (NR 3 LI), (NR 4 RI), (NR 5 MO) ])
  , (Node L, [ (NR 1 MI), (NR 1 LO), (NR 3 RO) ]) ]
