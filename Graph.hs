module Graph where

import qualified Data.List as L
import Port
import Atom
import Node


newtype Graph a = Graph { nodes :: [Node a] } deriving ( Show, Eq )


-- mkGraph :: Ord a => [Node a] -> Graph a  
-- mkGraph ns =
--   let
--     getConns n = filter $ \n' -> 
--       let
--         portsN  = ports n 
--         portsN' = ports n'
--       in any (\p -> any (isProperConn p) portsN) portsN' -- Nodes that share a port with n

--     mkConn n ns = (n, getConns n ns) 
--     connMap = map (\n -> mkConn n ns) ns
--   in
--     Graph $ M.fromList connMap

conns :: (Eq a) => Node a -> Graph a -> [Node a]
conns n g = filter (\n' -> connects n n') (nodes g)
  
-- conns n g = M.lookup n (unGraph g)


selectAtPort :: (Eq a, Ord a) => (Node a -> Maybe (Port a)) -> Node a -> Graph a -> Maybe (Node a)
selectAtPort port node graph = 
  case port node of 
    Nothing -> Nothing
    Just p  ->
      let 
        nodes = conns node graph
      in 
        L.find (\n -> any (isProperConn p) $ ports n) nodes
  

li :: (Eq a, Ord a) => Node a -> Graph a -> Maybe (Node a)
li = selectAtPort liPort

ri :: (Eq a, Ord a) => Node a -> Graph a -> Maybe (Node a)
ri = selectAtPort riPort

mi :: (Eq a, Ord a) => Node a -> Graph a -> Maybe (Node a)
mi = selectAtPort miPort

lo :: (Eq a, Ord a) => Node a -> Graph a -> Maybe (Node a)
lo = selectAtPort loPort

ro :: (Eq a, Ord a) => Node a -> Graph a -> Maybe (Node a)
ro = selectAtPort roPort

mo :: (Eq a, Ord a) => Node a -> Graph a -> Maybe (Node a)
mo = selectAtPort moPort

g = Graph
  [ lam 1 2 3
  , app 3 4 5
  ]
