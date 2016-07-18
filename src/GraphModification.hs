module GraphModification 
  ( GraphModification(..)
  , PatternMove
  , applyGraphModification
  , graphModifications -- rename this please
  , allGraphModifications
  , addToGraph
  )
  where

import Data.List ((\\))
import qualified Data.Map as M
import Data.Either
import Port
import Atom
import Node
import Graph
import Pattern
import Moves


data GraphModification a = GraphModification
  { removedNodes :: [Node a]
  , addedNodes   :: [NewNode a] }
  deriving ( Show )

-- replace this with a tuple with two inner type sigs
type PatternMove a = (Pattern a (Node a, Node a), (Node a, Node a) -> [NewNode a])

applyGraphModification :: (Eq a, Enum a) => GraphModification a -> Graph a -> Graph a
applyGraphModification mod graph = 
  let
    withRemoved = Graph $ nodes graph \\ removedNodes mod
    withAdded   = addToGraph (addedNodes mod) withRemoved 
  in withAdded

graphModifications 
  :: (Eq a) 
  => PatternMove a
  -> Graph a 
  -> [GraphModification a]
graphModifications (pattern, move) graph =  
  let 
    matches       = nodesOnce $ match pattern graph
    removedNodes  = map (\(n1,n2) -> [n1, n2]) matches
    addedNodes    = map move matches
    modifications = map (\(r,a) -> GraphModification r a) $ zipWith (,) removedNodes addedNodes
  in  
    modifications
     
-- Each list has all of the patternMoves of the same priority level
-- PatternMoves in the same list have the same priorty and act on the same Graph
-- Later patterns cannot match on nodes used in previous patterns
allGraphModifications
  :: Eq a
  => [[PatternMove a]]
  -> Graph a
  -> [GraphModification a]
allGraphModifications patternMoveLists graph = concat $ snd $ 
  foldl 
    (\(graph, mods) currentPatternMoves -> 
      let
        mods'  = currentPatternMoves >>= \pm -> graphModifications pm graph
        nodes' = nodes graph \\ (mods' >>= removedNodes)
      in
        (Graph nodes', mods':mods))
   (graph, []) -- accumulating the list of modifications
   patternMoveLists 


addToGraph :: (Eq a, Enum a) => [NewNode a] -> Graph a -> Graph a
addToGraph additions graph =
  let
    unused = unusedPortIds graph

    reifyPort p = fmap reify p 
      where
        reify (NewId a)    = unused !! a 
        reify (ActualId a) = a

    toAddableNode (Node a ps) = Node a $ map reifyPort ps

    properAdditions = map toAddableNode additions
  in 
    Graph $ (nodes graph) ++ properAdditions
