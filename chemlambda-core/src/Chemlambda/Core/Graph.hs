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

import qualified Data.List as L
import Data.List ((\\))
import Chemlambda.Core.Connectable
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node


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
  

minus :: Eq a => Graph [a] -> Graph [a] -> Graph [a]
g1 `minus` g2 = (\\) <$> g1 <*> g2

plus :: Eq a => Graph [a] -> Graph [a] -> Graph [a]
plus = mappend

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


connections :: (Eq a, Connectable a) => a -> Graph [a] -> [a]
connections node graph 
  | elem node (nodes graph) = filter (connects node) (nodes graph)
  | otherwise               = []

fromNode :: a -> Graph [a]
fromNode node = return [node]


type NodeSelector a = Node a -> Graph [Node a] -> Maybe (Node a)

selectNodeAtPort 
  :: Eq a 
  => (Node a -> Maybe (Port a)) 
  -> NodeSelector a 
selectNodeAtPort portSel node graph = 
  case portSel node of 
    Nothing   -> Nothing
    Just port ->
      let 
        conns = connections node graph
      in 
        L.find (\n -> any (connects port) $ ports n) conns 

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


unusedPortIds
  :: forall a. (Enum a, Ord a)
  => Graph [Node a]
  -> [a]
unusedPortIds graph = 
  let
    possible  = iterate succ (toEnum 0 :: a)
    inUse     = concatMap (map portId . ports) $ nodes graph
    unused    = tail $ iterate succ (maximum inUse)
    -- unused = filter (\p -> not $ elem p inUse) possible
    -- unused = possible \\ inUse
  in
    case graph of
      Graph [] -> possible
      _        -> unused

