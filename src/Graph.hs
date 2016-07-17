{-# LANGUAGE ScopedTypeVariables #-}

module Graph 
  ( Graph(..) 
  , NodeSelector
  , li, ri, mi, lo, ro, mo
  , connections
  , unusedPortIds
  )
  where

import qualified Data.List as List
import Data.List ((\\))
import Connectable
import Port
import Atom
import Node


newtype Graph a = Graph { nodes :: [Node a] } deriving ( Show, Eq )

connections :: Eq a => Node a -> Graph a -> [Node a]
connections node graph = filter (connects node) (nodes graph)

selectNodeAtPort 
  :: (Eq a, Ord a) 
  => (Node a -> Maybe (Port a)) 
  -> NodeSelector a 
selectNodeAtPort portSel node graph = 
  case portSel node of 
    Nothing -> Nothing
    Just p ->
      let 
        connectedNodes = connections node graph
      in 
        List.find (\n -> any (connects p) $ ports n) connectedNodes

  
type NodeSelector a = Node a -> Graph a -> Maybe (Node a)

li :: (Eq a, Ord a) => NodeSelector a
li = selectNodeAtPort liPort

ri :: (Eq a, Ord a) => NodeSelector a 
ri = selectNodeAtPort riPort

mi :: (Eq a, Ord a) => NodeSelector a
mi = selectNodeAtPort miPort

lo :: (Eq a, Ord a) => NodeSelector a
lo = selectNodeAtPort loPort

ro :: (Eq a, Ord a) => NodeSelector a
ro = selectNodeAtPort roPort

mo :: (Eq a, Ord a) => NodeSelector a
mo = selectNodeAtPort moPort


unusedPortIds :: forall a. (Eq a, Enum a) => Graph a -> [a]
unusedPortIds graph = 
  let
    possible = iterate succ (toEnum 0 :: a)
    unused   = possible \\ (concatMap (map portId . ports) $ nodes graph) 
  in
    case graph of
      Graph [] -> possible
      g        -> possible \\ (concatMap (map portId . ports) $ nodes graph)  
