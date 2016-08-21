module Chemlambda.Core.Graph
  where
 
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))

import Chemlambda.Core.Atom


data PortType = LO | LI | RO | RI | MO | MI
  deriving ( Eq, Show )

data Direction = I | O
  deriving ( Eq, Ord, Show )

data NodeRef a = NR { nRef :: a, nPT :: PortType } deriving ( Eq )

data Node a b = Node { atom :: a, refs :: [ NodeRef b ] }
  deriving ( Eq, Show )

newtype Graph a = Graph { unGraph :: IntMap (Node a Int) }
  deriving ( Eq )


instance Show a => Show (NodeRef a) where
  show (NR i pt) = "(NR " ++ show i ++ " " ++ show pt ++ ")"
 
instance Show a => Show (Graph a) where
  show graph = "Nodes\n" ++ IntMap.showTree (unGraph graph)


isOut :: PortType -> Bool
isOut = flip elem [LO, RO, MO]

isIn :: PortType -> Bool
isIn = flip elem [LI, RI, MI]

direction :: PortType -> Direction
direction p
  | isOut p = O
  | isIn  p = I


getNode :: Graph a -> Int -> (Node a Int) 
getNode = (!) . unGraph

addFrees :: [(Atom, [(PortType, Int)])] -> [(Atom, [(PortType, Int)])]
addFrees entries =
  let
    allPorts = concatMap snd entries

    withoutPair = 
      filter 
        (\(p,i) 
          -> List.notElem i                               
          $ map snd                                      
          $ allPorts List.\\ [(p,i)])
        allPorts

    frees =
      map
        (\(p,i) -> case direction p of
          O -> (FROUT, [(MI,i)])
          I -> (FRIN,  [(MO,i)]))
        withoutPair
  in entries ++ frees

toGraph :: [(Atom, [(PortType, Int)])] -> Graph Atom
toGraph entries =
  let
    entries' = addFrees entries
    indexedAtoms = zip [0..] (map fst entries') 
    indexedPorts = concat $ zipWith (\i ps -> map ((,) i) ps) [0..] (map snd entries')

    toConnectedPorts (a,ps) = 
      let
        ps' = foldr 
          (\(j,(p,i)) ps' -> 

            if elem i $ map snd $ ps List.\\ [(p,i)] 
              then let
                matchingPort = (\(p,_) -> NR j p) <$> List.find (\(_,i') -> i' == i) ps
                in Maybe.fromJust matchingPort : ps' 
              else ps')

          []
          indexedPorts
      in Node a ps'
    
    graph = Graph
          $ IntMap.fromList
          $ zip [0..]
          $ map toConnectedPorts
          $ entries'
  in graph

test = toGraph  
  [ ( L, [ (MI,1), (LO,1), (RO,2) ] ) 
  , ( A, [ (LI,2), (RI,3), (MO,4) ] ) ]
