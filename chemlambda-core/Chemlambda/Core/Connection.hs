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

data NodeRef a = NR { nRef :: a, nPT :: PortType } deriving ( Eq )
data EdgeRef a = ER { eRef :: a, ePT :: PortType } deriving ( Eq )

data Node a = Node { unNode :: a }
  deriving ( Eq )

data Edge a = Edge a a
  deriving ( Eq, Show )

data Graph = Graph
  { graphNodes :: IntMap (Node Atom,[EdgeRef Int])
  , graphEdges :: IntMap (Edge (NodeRef Int)) }
  deriving ( Eq, Show )

instance Show a => Show (NodeRef a) where
  show (NR i pt) = show (i,pt)
 
instance Show a => Show (EdgeRef a) where
  show (ER i pt) = show (i,pt)

instance Show a => Show (Node a) where
  show (Node a) = "Node " ++ show a

instance Functor Edge where
  fmap f (Edge a b) = Edge (f a) (f b)

isOut :: PortType -> Bool
isOut = flip elem [LO, RO, MO]

isIn :: PortType -> Bool
isIn = flip elem [LI, RI, MI]

direction :: PortType -> Direction
direction p
  | isOut p = O
  | isIn  p = I


getNode :: Graph -> Int -> (Node Atom,[EdgeRef Int]) 
getNode = (!) . graphNodes

getEdge :: Graph -> Int -> (Edge (NodeRef Int))
getEdge = (!) . graphEdges

portGroups :: [(n,[(NodeRef Int)])] -> [[NodeRef Int]]
portGroups entries =
  let
    portEntries = sortBy  (\ p q -> compare (nRef p) (nRef q)) $ concatMap snd entries
    groups      = groupBy (\ p q -> nRef p == nRef q) portEntries
  in
   groups


addFrees :: [( Node Atom, [NodeRef Int] )] -> [( Node Atom, [NodeRef Int] )]
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


toGraph :: [( Node Atom, [NodeRef Int] )] -> Graph
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

toAtomPort :: (NodeRef Int) -> Graph -> (NodeRef Atom)
toAtomPort (NR i pt) g = NR (unNode $ fst $ getNode g i) pt

-- -- toAtomEdge :: (Edge (NodeRef Int)) -> Graph -> Edge (Atom,PortType)
-- -- toAtomEdge (Edge apA apB) g = Edge (toAtomPort apA g) (toAtomPort apB g)

test :: [(Node Atom, [NodeRef Int])]
test =
  [ (Node L, [ (NR 1 MI), (NR 1 LO), (NR 3 RO) ])
  , (Node A, [ (NR 3 LI), (NR 4 RI), (NR 5 MO) ]) ]
