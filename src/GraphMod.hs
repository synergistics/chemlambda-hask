module GraphMod where

import qualified Data.List as L
import Data.List ((\\))
import qualified Data.Map as M
import Data.Either
import Port
import Atom
import Node
import Graph
import Pattern
import Moves


data GraphMod a = GraphMod
  { removedNodes :: [Node a]
  , addedNodes   :: [GraphAddition a] }
  deriving ( Show )

type PatternMove a = (Pattern a (Node a, Node a), (Node a, Node a) -> [GraphAddition a])

applyGraphMod :: (Eq a, Enum a) => GraphMod a -> Graph a -> Graph a
applyGraphMod mod graph = 
  let
    withRemoved = Graph $ nodes graph \\ removedNodes mod
    withAdded   = addToGraph (addedNodes mod) withRemoved 
  in withAdded

graphMods 
  :: (Eq a) 
  => PatternMove a
  -> Graph a 
  -> [GraphMod a]
graphMods (pattern, move) graph =  
  let 
    matches       = match pattern graph
    removedNodes  = map (\(n1,n2) -> [n1, n2]) matches
    addedNodes    = map move matches
    modifications = map (\(r,a) -> GraphMod r a) $ zipWith (,) removedNodes addedNodes
  in  
    modifications
     
-- Each list has all of the patternMoves of the same priority level
-- PatternMoves in the same list have the same priorty and act on the same Graph
-- Later patterns cannot match on nodes used in previous patterns
allGraphMods
  :: Eq a
  => [[PatternMove a]]
  -> Graph a
  -> [GraphMod a]
allGraphMods patternMoveLists graph = concat $ snd $ 
  foldl 
    (\(graph, mods) currentPatternMoves -> 
      let
        mods'  = concatMap (\pm -> graphMods pm graph) currentPatternMoves
        nodes' = nodes graph \\ concatMap removedNodes mods'
      in
        (Graph $ nodes', mods' : mods))
   (graph, [])
   patternMoveLists 


addToGraph :: (Eq a, Enum a) => [GraphAddition a] -> Graph a -> Graph a
addToGraph additions graph =
  let
    unused = unusedPortIds graph

    reifyPort p = reify <$> p
      where
        reify (Left (New a)) = unused !! a 
        reify (Right a)      = a

    toAddableNode (Node x ps) = Node x $ map reifyPort ps

    properAdditions = map toAddableNode additions
  in 
    Graph $ (nodes graph) ++ properAdditions
