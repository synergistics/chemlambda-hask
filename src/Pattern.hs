module Pattern
  ( Pattern(..)
  , match
  -- , nodesOnce
  , anyNode, atomOf, nodeOf
  , conn
  , betaPattern
  , combPattern
  , fanInPattern
  , distLPattern
  , distAPattern
  , distFIPattern
  , distFOPattern
  , prunePatterns
  )
  where

import Control.Applicative
import Data.Maybe
import Port
import Atom
import Node
import Graph

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


-- Chemlambda Patterns
betaPattern   :: Eq a => Pattern [Node a] (Graph [Node a])
betaPattern   = conn (atomOf L) (atomOf A) [roPort] [liPort]

combPattern   :: Eq a => Pattern [Node a] (Graph [Node a])
combPattern   = conn anyNode (atomOf ARROW) [loPort,roPort,moPort] [miPort]

fanInPattern  :: Eq a => Pattern [Node a] (Graph [Node a])
fanInPattern  = conn (atomOf FI) (atomOf FOE) [moPort] [miPort]

distLPattern  :: Eq a => Pattern [Node a] (Graph [Node a])
distLPattern  = conn (atomOf L) (atomOf FO <|> atomOf FOE) [roPort] [miPort]

distAPattern  :: Eq a => Pattern [Node a] (Graph [Node a])
distAPattern  = conn (atomOf A) (atomOf FO <|> atomOf FOE) [moPort] [miPort]

distFIPattern :: Eq a => Pattern [Node a] (Graph [Node a])
distFIPattern = conn (atomOf FI) (atomOf FO) [moPort] [miPort]

distFOPattern :: Eq a => Pattern [Node a] (Graph [Node a])
distFOPattern = conn (atomOf FO) (atomOf FOE) [roPort] [miPort]

prunePatterns :: Eq a => Pattern [Node a] (Graph [Node a])
prunePatterns = foldl1 (<|>) patterns
  where
    patterns =
      [ conn (atomOf A)   (atomOf T) [moPort] [miPort]
      , conn (atomOf FI)  (atomOf T) [moPort] [miPort]
      , conn (atomOf L)   (atomOf T) [loPort] [miPort]
      , conn (atomOf FO)  (atomOf T) [loPort] [miPort]
      , conn (atomOf FOE) (atomOf T) [loPort] [miPort]
      , conn (atomOf FO)  (atomOf T) [roPort] [miPort]
      , conn (atomOf FOE) (atomOf T) [roPort] [miPort]
      ]
