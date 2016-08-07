{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Chemlambda.Core.Graph
  ( Graph(..) 
  , NodeSelector
  , li, ri, mi, lo, ro, mo
  , connections
  , minus
  , plus
  , plusNew
  , unusedPortIds
  )
  where

import Data.List
import Chemlambda.Core.Connectable
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node


-- | A Graph will in most cases be a list of nodes
-- This is my least favorite part of my implementation
-- todo: Make the Graph type have more structure, maybe try a map eventually 
newtype Graph a = Graph { nodes :: a } deriving ( Show, Eq )

instance Functor Graph where
  fmap f (Graph graph) = Graph $ f graph 

instance Applicative Graph where
  pure a = Graph a
  Graph f <*> graph = fmap f $ graph

instance Monad Graph where
  return = pure
  Graph graph >>= f = f graph

instance Monoid (Graph [a]) where
  mempty = Graph []
  mappend graphA graphB = (++) <$> graphA <*> graphB
  

-- | @minus@ takes the left outer join on graphs
minus :: Eq a => Graph [a] -> Graph [a] -> Graph [a]
g1 `minus` g2 = (\\) <$> g1 <*> g2

-- | @plus@ takes the union of graphs
plus :: Eq a => Graph [a] -> Graph [a] -> Graph [a]
plus = mappend

-- | @plusNew@ adds 'Node's holding 'Port's with 'NewId's and adds them to a graph 
-- @NewId Int@s are reified by replacing their port number with an unused port number in the starting graph
-- @ActualId a@s are reified by adding their inner port id without modification
plusNew 
  :: (Enum a, Ord a)
  => Graph [Node a] 
  -> Graph [Node (NewId a)] 
  -> Graph [Node a]
plusNew graph graphNew = 
  let
    unusedIds = unusedPortIds graph
    
    reifyPort port = fmap go port
      where
        go (NewId a)    = unusedIds !! a
        go (ActualId a) = a
    
    reifyNode node = node { ports = map reifyPort (ports node) }

    graph' = map reifyNode <$> graphNew
  in
    graph `plus` graph'


-- | @connections@ returns the list of nodes connected to a node in a graph
connections :: (Eq a) => Node a -> Graph [Node a] -> [Node a]
connections node graph 
  | elem node (nodes graph) = filter (connects node) (nodes graph)
  | otherwise               = []


-- | See, stupid.
fromNode :: a -> Graph [a]
fromNode node = return [node]


-- | Possibly selects a node given a graph
type NodeSelector a = Node a -> Graph [Node a] -> Maybe (Node a)

-- | Makes a NodeSelector for a node at a given port of another
selectNodeAtPort 
  :: Eq a 
  => (Node a -> Maybe (Port a)) 
  -> NodeSelector a 
selectNodeAtPort portSel node graph = portSel node >>= findNode
  where
    findNode port = 
      let conns = connections node graph
      in find (\n -> any (connects port) $ ports n) conns 


-- === Node selectors
li :: Eq a => NodeSelector a
li = selectNodeAtPort liPort

ri :: Eq a => NodeSelector a 
ri = selectNodeAtPort riPort

mi :: Eq a => NodeSelector a
mi = selectNodeAtPort miPort

lo :: Eq a => NodeSelector a
lo = selectNodeAtPort loPort

ro :: Eq a => NodeSelector a
ro = selectNodeAtPort roPort

mo :: Eq a => NodeSelector a
mo = selectNodeAtPort moPort


-- | Returns a list of unused portIds in a graph
unusedPortIds
  :: forall a. (Enum a, Ord a)
  => Graph [Node a]
  -> [a]
unusedPortIds graph = 
  let
    possible = iterate succ (toEnum 0 :: a)
    inUse    = concatMap (map portId . ports) $ nodes graph
    unused   = tail $ iterate succ (maximum inUse)
  in
    case graph of
      Graph [] -> possible
      _        -> unused

