module Graph where

import qualified Data.List as L
import Port
import Atom
import Node


newtype Graph a = Graph { nodes :: a } deriving ( Show, Eq )

instance Functor Graph where
  fmap f graph = Graph $ f (nodes graph)

instance Applicative Graph where
  pure a = Graph a
  fGraph <*> graph = Graph (nodes fGraph $ nodes graph)

instance Monad Graph where
  return = pure
  graph >>= f = f $ nodes graph


conns :: (Eq a) => Node a -> Graph [Node a] -> [Node a]
conns n g = filter (\n' -> connects n n') (nodes g)
  

selectAtPort :: (Eq a, Ord a) => (Node a -> Maybe (Port a)) -> Node a -> Graph [Node a] -> Maybe (Node a)
selectAtPort port node graph = 
  case port node of 
    Nothing -> Nothing
    Just p  ->
      let 
        nodes = conns node graph
      in 
        L.find (\n -> any (isProperConn p) $ ports n) nodes
  

type NodeSel a = Node a -> Graph [Node a] -> Maybe (Node a)

li :: (Eq a, Ord a) => NodeSel a
li = selectAtPort liPort

ri :: (Eq a, Ord a) => NodeSel a 
ri = selectAtPort riPort

mi :: (Eq a, Ord a) => NodeSel a
mi = selectAtPort miPort

lo :: (Eq a, Ord a) => NodeSel a
lo = selectAtPort loPort

ro :: (Eq a, Ord a) => NodeSel a
ro = selectAtPort roPort

mo :: (Eq a, Ord a) => NodeSel a
mo = selectAtPort moPort

g = Graph
  [ lam 1 2 3
  , arrow 3 4 
  , t 2
  , lam 4 1 5
  , app 5 6 7
  , lam 8 8 6 
  , fi 7 9 10
  , fo 10 11 12
  ]
