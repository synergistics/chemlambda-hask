{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import qualified Data.List as L
import Port
import Atom
import Node


newtype Graph a = Graph { nodes :: [Node a] } deriving ( Show, Eq )


-- The list of nodes that connect to a node in a graph
conns :: (Eq a) => Node a -> Graph a -> [Node a]
conns n g = filter (\n' -> connects n n') (nodes g)
  

-- Generalized Node selector
type NodeSel a = Node a -> Graph a -> Maybe (Node a)

selectAtPort 
  :: (Eq a, Ord a) 
  => (Node a -> Maybe (Port a)) 
  -> NodeSel a 
selectAtPort portSel node graph = 
  case portSel node of 
    Nothing -> Nothing
    Just p  ->
      let 
        nodes = conns node graph
      in 
        -- Find the nodes that form a connection node at any port
        L.find (\n -> any (isProperConn p) $ ports n) nodes
  
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


unusedPortIds :: forall a. (Eq a, Enum a) => Graph a -> [a]
unusedPortIds graph = 
  let
    possible = iterate succ (toEnum 0 :: a)
    unused   = possible L.\\ (concatMap (map portId . ports) $ nodes graph) 
  in
    unused

g = Graph
  [ lam 1 2 3
  , app 3 4 5
  , fo  5 6 7
  ]
