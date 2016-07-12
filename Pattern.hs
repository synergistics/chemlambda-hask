module Pattern where

import Data.Maybe
import Port
import Atom
import Node
import Graph

data Pattern a b = Pattern (Graph a -> [b])

instance Functor (Pattern a) where
  fmap f p = Pattern $ \g -> map f $ match p g

instance Applicative (Pattern a) where
  pure a  = Pattern $ const [a]
  f <*> p = Pattern $ \g ->
    do
      f' <- match f g
      p' <- match p g
      return $ f' p'

instance Monad (Pattern a) where
  return  = pure
  p >>= f = Pattern $ \g -> concatMap (\m -> match (f m) g) $ match p g

match :: Pattern a b -> Graph a -> [b]
match (Pattern p) g = p g

-- Matches either pattern
or :: Pattern a b -> Pattern a b -> Pattern a b
or patternA patternB = Pattern $ \graph -> match patternA graph ++ match patternB graph

-- Matches any Node
anyNode :: Pattern a (Node a)
anyNode = Pattern nodes 

-- Matches Nodes with an atom of 'a'
atomOf :: Atom -> Pattern a (Node a)
atomOf a = Pattern $ \g ->
  filter (\n -> atom n == a) (nodes g)


-- Matches Nodes equivalent to 'n'
nodeOf :: (Eq a) => Node a -> Pattern a (Node a)
nodeOf n = Pattern $ \g -> filter (== n) (nodes g)

-- Combines two patterns by finding a connection between the nodes they return
conn :: 
  (Eq a) => 
  Pattern a (Node a) -> 
  Pattern a (Node a) -> 
  [NodeSel a] -> 
  [NodeSel a] -> 
  Pattern a (Node a, Node a)
conn patternA patternB portsA portsB = Pattern $ \graph -> 
  let 
    portPairs = do
      a <- portsA
      b <- portsB
      return (a,b)

    matchesA = match patternA graph
    matchesB = match patternB graph
    matchPairs = do
      a <- matchesA
      b <- matchesB
      return (a,b) 

    -- The possible sets of nodes and ports that form a connection
    connGroups = do
      (a,b) <- matchPairs
      (p,q) <- portPairs
      return (a,b,p,q)

    -- Returns whether portP of nodeA connects to portQ of nodeB
    connects nodeA nodeB portP portQ graph = 
      portP nodeA graph == Just nodeB && 
      portQ nodeB graph == Just nodeA
  in
    -- Return just the nodes
    map (\(a,b,p,q) -> (a,b)) $ filter (\(a,b,p,q) -> connects a b p q graph) connGroups


type ConnPattern a = Pattern a (Node a, Node a)

-- Chemlambda Patterns
betaPattern :: (Ord a, Eq a) => ConnPattern a 
betaPattern = conn (atomOf L) (atomOf A) [ro] [li] 

combPattern :: (Ord a, Eq a) => ConnPattern a
combPattern = conn anyNode (atomOf ARROW) [lo,ro,mo] [mi]

fanInPattern :: (Ord a, Eq a) => ConnPattern a
fanInPattern = conn (atomOf FI) (atomOf FOE) [mo] [mi]

distLPattern :: (Ord a, Eq a) => ConnPattern a
distLPattern = conn (atomOf L) (atomOf FO `Pattern.or` atomOf FOE) [ro] [mi]

distAPattern :: (Ord a, Eq a) => ConnPattern a
distAPattern = conn (atomOf A) (atomOf FO `Pattern.or` atomOf FOE) [mo] [mi]

distFIPattern :: (Ord a, Eq a) => ConnPattern a
distFIPattern = conn (atomOf FI) (atomOf FO) [mo] [mi]

distFIPattern :: (Ord a, Eq a) => ConnPattern a
distFIPattern = conn (atomOf FO) (atomOf FOE) [ro] [mi]
