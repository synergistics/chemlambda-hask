module Reduction where

import qualified Data.List as L
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
  , addedNodes   :: [GraphAddition a] }
  deriving ( Show )

graphMods 
  :: (Eq a) 
  => Pattern a (Node a, Node a)
  -> ((Node a, Node a) -> [GraphAddition a])
  -> Graph a 
  -> [GraphModification a]
graphMods pattern move graph =  
  let 
    matches       = match pattern graph
    removedNodes  = map (\(n1,n2) -> [n1, n2]) matches
    addedNodes    = map move matches
    modifications = map (\(r,a) -> GraphModification r a) $ zipWith (,) removedNodes addedNodes
  in  
    modifications
     
go pms graph = snd $ 
  foldl 
    (\(graph, mods) (pattern, move) -> 
      let
        mods' = graphMods pattern move graph
      in
        (Graph $ (nodes graph) L.\\ (concatMap removedNodes mods'), mods' : mods))
   (graph, [])
   pms 


-- --addToGraph :: GraphAddition a -> Graph a -> Graph a
-- addToGraph addition graph =
  -- let
  --   unused = unusedPortIds graph

  --   getNewPortIds :: Eq a => GraphAddition a -> [Int] 
  --   getNewPortIds ns = L.nub $ map (\(Left (New n)) -> n) $ concatMap (\n -> filter isLeft (map portId $ ports n)) ns

  --   reifyPort p = reify <$> p
  --     where
  --       reify (Left (New a)) = unused !! a 
  --       reify (Right a)      = a

  --   toAddableNode (Node x ps) = Node x $ map reifyPort ps

  --   properAddition = map toAddableNode addition
  -- in 
  --   properAddition
  --   -- getNewPortIds addition
  --   -- unusedIds = unusedPortIds graph

  --   -- portToUnused = Map.empty 

  --   -- toAcceptableNode (Node a ps) = Node a $ map toAcceptablePort ps 
    
-- -- moveCycle
-- --   :: Eq a
-- --   => [(Pattern a (Node a, Node a), 
-- --       (Node a, Node a) -> GraphAddition a)] 
-- --   -> Graph a 
-- --   -> Graph a
-- -- moveCycle patterns graph =
  

