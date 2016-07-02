module Chemlambda.Selectors where

import Data.List 

import Chemlambda.Node
import qualified Chemlambda.Graph as C
import qualified Util.Graph as G

-- Returns the element of a graph that has the incoming edge of e
withInEdge :: Int -> C.Graph -> Maybe C.View
withInEdge e g = (G.:& g) <$> find (\c -> elem e (G.inEdges c)) g

withOutEdge :: Int -> C.Graph -> Maybe C.View
withOutEdge e g = (G.:& g) <$> find (\c -> elem e (G.outEdges c)) g

{- 
 - The first Int denotes whether to look at an incoming or outgoing port
 - The second Int denotes which element of the incoming or outgoing ports to grab (left or right)
-}
type PortSel = (Int,Int)

-- Returns the selected port num of a node
mkRef :: [Node] -> PortSel -> C.View -> Maybe Int
mkRef nodes sel v@(c G.:& g) =
  if elem (G.node c) nodes
    then case fst sel of
      0 -> Just $ G.inEdges c !! snd sel
      1 -> Just $ G.outEdges c !! snd sel
    else Nothing 

-- So e.g moRef (app 1 2 3 :& someGraph) -> Just 3
moRef :: C.View -> Maybe Int
moRef = mkRef [A,FI,FRIN,Arrow] (1,0)

miRef :: C.View -> Maybe Int
miRef = mkRef [L,FO,FOE,FROUT,Arrow,T] (0,0)

loRef :: C.View -> Maybe Int
loRef = mkRef [L,FO,FOE] (1,0)

liRef :: C.View -> Maybe Int
liRef = mkRef [A,FI] (0,0)

roRef :: C.View -> Maybe Int
roRef = mkRef [L,FO,FOE] (1,1)

riRef :: C.View -> Maybe Int
riRef = mkRef [A,FI] (0,1)


type Selector = C.View -> Maybe C.View

-- Return the view of the node at the selected port of a view
mo :: Selector 
mo v@(c G.:& g) = 
  -- E.g. find the context with an incoming edge of whatever is coming out of this one
  moRef v >>= (\edgeRef -> withInEdge edgeRef g)

mi :: Selector
mi v@(c G.:& g) = 
  miRef v >>= (\edgeRef -> withOutEdge edgeRef g)

lo :: Selector
lo v@(c G.:& g) = 
  loRef v >>= (\edgeRef -> withInEdge edgeRef g)

li :: Selector
li v@(c G.:& g) = 
  liRef v >>= (\edgeRef -> withOutEdge edgeRef g)

ro :: Selector
ro v@(c G.:& g) = 
  roRef v >>= (\edgeRef -> withInEdge edgeRef g)

ri :: Selector
ri v@(c G.:& g) = 
  riRef v >>= (\edgeRef -> withOutEdge edgeRef g)

