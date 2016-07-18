{-# LANGUAGE ScopedTypeVariables #-}

module Graph where
  -- ( Graph(..) 
  -- , NodeSelector
  -- , li, ri, mi, lo, ro, mo
  -- , connections
  -- , unusedPortIds
  -- ) where
import qualified Data.List as List
import Data.List ((\\))
import Connectable
import Port
import Atom
import Node


newtype Graph a = Graph { nodes :: a } deriving ( Show, Eq )

instance Functor Graph where
  fmap f (Graph graph) = Graph $ f graph 

instance Applicative Graph where
  pure a = Graph a
  Graph f <*> graph = fmap f $ graph

instance Monad Graph where
  return = pure
  Graph graph >>= f = f graph


connections :: (Eq a, Connectable a) => a -> Graph [a] -> [a]
connections node graph = filter (connects node) (nodes graph)

fromNode :: a -> Graph [a]
fromNode node = Graph [node]


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
        List.find (\n -> any (connects port) $ ports n) conns 

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


unusedPortIds :: forall a. (Eq a, Enum a) => Graph [Node a] -> [a]
unusedPortIds graph = 
  let
    possible = iterate succ (toEnum 0 :: a)
    unused   = possible \\ (concatMap (map portId . ports) $ nodes graph) 
  in
    case graph of
      Graph [] -> possible
      _        -> unused 

