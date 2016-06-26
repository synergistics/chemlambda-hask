module Chemlambda.Graph where

import Data.List 
import qualified Util.Graph as G
import Chemlambda.Node
import Util.Pattern


-- Maybe make these polymorphic on the edge type
type Graph   = G.Graph Node Int
type Context = G.Context Node Int
type View    = G.View Node Int


-- Returns the element of a graph that has the incoming edge of e
withInEdge :: Int -> Graph -> Maybe View
withInEdge e g = (G.:& g) <$> find (\c -> elem e (G.inEdges c)) g

withOutEdge :: Int -> Graph -> Maybe View
withOutEdge e g = (G.:& g) <$> find (\c -> elem e (G.outEdges c)) g


-- (InOrOut, MiddleLeftOrRight)
type Selector = (Int,Int)
-- Returns the selected port number of a node
mkRef :: [Node] -> Selector -> View -> Maybe Int
mkRef nodes sel v@(c G.:& g) =
  if elem (G.node c) nodes
    then case fst sel of
      0 -> Just $ G.inEdges c !! snd sel
      1 -> Just $ G.outEdges c !! snd sel
    else Nothing 

-- So e.g moRef (app 1 2 3 :& someGraph) -> Just 3
moRef :: View -> Maybe Int
moRef = mkRef [A,FI,FRIN,Arrow] (1,0)

miRef :: View -> Maybe Int
miRef = mkRef [L,FO,FOE,FROUT,Arrow,T] (0,0)

loRef :: View -> Maybe Int
loRef = mkRef [L,FO,FOE] (1,0)

liRef :: View -> Maybe Int
liRef = mkRef [A,FI] (0,0)

roRef :: View -> Maybe Int
roRef = mkRef [L,FO,FOE] (1,1)

riRef :: View -> Maybe Int
riRef = mkRef [A,FI] (0,1)

-- Return the view of the node at the selected port of a view
mo :: View -> Maybe View
mo v@(c G.:& g) = 
  moRef v >>= (\edgeRef -> withInEdge edgeRef g)

mi :: View -> Maybe View
mi v@(c G.:& g) = 
  miRef v >>= (\edgeRef -> withOutEdge edgeRef g)

lo :: View -> Maybe View
lo v@(c G.:& g) = 
  loRef v >>= (\edgeRef -> withInEdge edgeRef g)

li :: View -> Maybe View
li v@(c G.:& g) = 
  liRef v >>= (\edgeRef -> withOutEdge edgeRef g)

ro :: View -> Maybe View
ro v@(c G.:& g) = 
  roRef v >>= (\edgeRef -> withInEdge edgeRef g)

ri :: View -> Maybe View
ri v@(c G.:& g) = 
  riRef v >>= (\edgeRef -> withOutEdge edgeRef g)
