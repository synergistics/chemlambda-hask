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
  deriving ( Eq )


instance Show a => Show (NodeRef a) where
  show (NR i pt) = "(NR " ++ show i ++ " " ++ show pt ++ ")"
 
instance Show a => Show (EdgeRef a) where
  show (ER i pt) = "(ER " ++ show i ++ " " ++ show pt ++ ")"

instance Show a => Show (Node a) where
  show (Node a) = "Node " ++ show a

instance Show Graph where
  show graph = "Nodes\n" ++ IntMap.showTree (graphNodes graph) ++ "\nEdges\n" ++ IntMap.showTree (graphEdges graph)

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

groupByNode :: [NodeRef Int] -> [[NodeRef Int]]
groupByNode entries =
  let
    portEntries = sortBy  (\ p q -> compare (nRef p) (nRef q)) entries 
    groups      = groupBy (\ p q -> nRef p == nRef q) portEntries
  in
   groups


addFrees :: [( Node Atom, [NodeRef Int] )] -> [( Node Atom, [NodeRef Int] )]
addFrees entries =
  let
    portSingletons
      = map head
      $ filter ((== 1) . length)
      $ groupByNode 
      $ concatMap snd entries

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


refWithAtom :: NodeRef Int -> Graph -> (NodeRef Atom)
refWithAtom (NR i pt) g = NR (unNode $ fst $ getNode g i) pt

refWithAtomAndNum :: NodeRef Int -> Graph -> (NodeRef (Int, Atom))
refWithAtomAndNum (NR i pt) g = NR (i, unNode $ fst $ getNode g i) pt

edgeWithAtoms :: (Edge (NodeRef Int)) -> Graph -> Edge (NodeRef Atom)
edgeWithAtoms (Edge apA apB) g = Edge (refWithAtom apA g) (refWithAtom apB g)

edgeWithAtomsAndNums :: (Edge (NodeRef Int)) -> Graph -> Edge (NodeRef (Int, Atom))
edgeWithAtomsAndNums (Edge apA apB) g = Edge (refWithAtomAndNum apA g) (refWithAtomAndNum apB g)


-- | Return the edge connected to node at a given port along with its index in the graph
edgeAtPort :: NodeRef Int -> Graph -> Maybe (Int, Edge (NodeRef Int))
edgeAtPort nr g =
  let
    (_, ers) = getNode g (nRef nr)
    -- find the edgeRef with the correct port type
    edgeRef  = fmap eRef $ find (\er -> ePT er == nPT nr) ers
    edge     = fmap (getEdge g) edgeRef
  in
    (,) <$> edgeRef <*> edge

-- | Return the node connected to another node at a given port along with its index in the graph
nodeAtPort :: NodeRef Int -> Graph -> Maybe (Int, (Node Atom, [EdgeRef Int]))
nodeAtPort nr g = 
  let
    getNodeRef (Edge a b) = 
      if nPT nr == nPT a
      then (nRef b, getNode g (nRef b))
      else (nRef a, getNode g (nRef a))

    edge = edgeAtPort nr g
    node = fmap (getNodeRef . snd) edge
  in
    node

lam :: Int -> Int -> Int -> (Node Atom, [NodeRef Int])
lam a b c = (Node L, [ (NR a MI), (NR b LO), (NR c RO) ])

app :: Int -> Int -> Int -> (Node Atom, [NodeRef Int])
app a b c = (Node A, [ (NR a LI), (NR b RI), (NR c MO) ])

test = toGraph  
  [ lam 1 1 3 
  , app 3 4 5 ]
