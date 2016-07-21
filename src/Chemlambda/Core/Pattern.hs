module Chemlambda.Core.Pattern
  ( Pattern(..)
  , match
  -- , nodesOnce
  , anyNode, atomOf, nodeOf
  , conn
  )
  where

import Control.Applicative
import Data.Maybe
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.Graph

-- Usually matches a subgraph of a graph
newtype Pattern a b = Pattern (Graph a -> [b])

instance Functor (Pattern a) where
  fmap f p = Pattern $ \graph -> map f $ match p graph

instance Applicative (Pattern a) where
  pure a  = Pattern $ const [a]
  f <*> p = Pattern $ \graph ->
    do
      f' <- match f graph
      p' <- match p graph
      return $ f' p'

instance Alternative (Pattern a) where
  empty = Pattern (pure [])
  patternA <|> patternB = Pattern $ \graph -> match patternA graph ++ match patternB graph

instance Monad (Pattern a) where
  return  = pure
  p >>= f = Pattern $ \graph -> concatMap (\res -> match (f res) graph) $ match p graph


match :: Pattern a b -> Graph a -> [b]
match (Pattern applyPattern) graph = applyPattern graph


-- Figure out how to handle this later
-- nodesOnce :: Eq a => [Graph [Node a]] -> [Graph [Node a]]
-- nodesOnce nodes =
--   let
--     nodeIn node graph = elem node (nodes graph) 
--   in
--     foldr
--       (\node acc ->
--         if nodeIn node acc
--           then acc
--           else node : acc)
--       []
--       nodes


-- Matches any Node
anyNode :: Pattern [Node a] (Node a)
anyNode = Pattern nodes

-- Matches Nodes with an atom of 'a'
atomOf :: Atom -> Pattern [Node a] (Node a)
atomOf a = Pattern $ filter (\node -> a == atom node) . nodes

nodeOf :: (Eq a) => Node a -> Pattern [Node a] (Node a)
nodeOf n = Pattern $ filter (== n) . nodes

-- Combines two patterns on Nodes
-- Matches on a connection between the Nodes they return
conn
  :: Eq a
  => Pattern [Node a] (Node a)
  -> Pattern [Node a] (Node a)
  -> [PortSel a]
  -> [PortSel a]
  -> Pattern [Node a] (Graph [Node a])
conn patternA patternB portsA portsB = Pattern $ \graph ->
  let
    portPairs = do
      a <- portsA
      b <- portsB
      return (a,b)

    connectsAtPorts nodeA nodeB portP portQ graph =
      (connects <$> portP nodeA <*> portQ nodeB) == (Just True)

    matchPairs = do
      node     <- match patternA graph
      connNode <- match patternB $ Graph $ connections node graph
      return (node, connNode)

    connGroups = do
      (a,b) <- matchPairs
      (p,q) <- portPairs
      return (a,b,p,q)

  -- in matchPairs
  in map (\(a,b,p,q) -> Graph [a,b]) $ filter (\(a,b,p,q) -> connectsAtPorts a b p q graph) connGroups
