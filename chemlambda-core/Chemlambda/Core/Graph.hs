module Chemlambda.Core.Graph
  where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))

import Chemlambda.Core.Atom
import Chemlambda.Core.Port
import Chemlambda.Core.Node


newtype Graph a = Graph { unGraph :: IntMap (Node a Int) }
  deriving ( Eq )

instance Show a => Show (Graph a) where
  show graph = "Nodes\n" ++ IntMap.showTree (unGraph graph) 

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

subGraph :: [Int] -> Graph a -> Graph a
subGraph elems graph =
  let
    selectedNodes
      = filter (\(i,_) -> elem i elems)
      $ IntMap.assocs
      $ unGraph graph

  in Graph $ IntMap.fromList selectedNodes

nodeAtPort :: Node Atom Int -> PortType -> Graph Atom -> Maybe (Node Atom Int)
nodeAtPort node pt graph
  = getNode graph
  <$> nRef
  <$> refAtPort node pt

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
            
            let withoutCurrentPort = ps List.\\ [(p,i)] in

            if elem i $ map snd $ withoutCurrentPort
              then let
                matchingPort = (\(p,_) -> NR j p) <$> List.find (\(_,i') -> i' == i) withoutCurrentPort
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

lam a b c = (L,  [ (MI,a), (LO,b), (RO,c) ])
fo  a b c = (FO, [ (MI,a), (LO,b), (RO,c) ])
app a b c = (A,  [ (LI,a), (RI,b), (MO,c) ])

test
  = toGraph
  $ take 500
  $ concat
  $ iterate (map (\(a, [(x,xx), (y,yy), (z,zz)]) -> (a, [(x,xx+200),(y,yy+200),(z,zz+200)]))) 
  [ lam 1 1 2 
  , app 2 3 4 
  , fo  8 9 10 
  , lam 13 13 111
  , app 111 43 44 ]

