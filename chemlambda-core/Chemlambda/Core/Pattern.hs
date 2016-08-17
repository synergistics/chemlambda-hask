{-# LANGUAGE GADTs #-}
module Chemlambda.Core.Pattern
  where

-- import qualified Data.IntMap as IntMap
-- import Chemlambda.Core.Atom
-- import Chemlambda.Core.Connection


-- data Pattern a where
--   AnyP      :: Pattern a
--   PortP     :: PortType -> Pattern PortType
--   OutPortP  :: Pattern PortType
--   InPortP   :: Pattern PortType
--   AtomP     :: Atom -> Pattern Atom
--   AtomPortP :: (Pattern Atom, Pattern PortType) -> Pattern (Atom,PortType)
--   EdgeP     :: Edge (Pattern (Atom,PortType)) -> Pattern (Edge (Atom,PortType))

-- type EdgePattern = Pattern (Edge (Atom,PortType)) 

-- runPattern :: Pattern a -> a -> Bool
-- runPattern AnyP                   = const True
-- runPattern (PortP p)              = (==) p
-- runPattern OutPortP               = (== O) . direction
-- runPattern InPortP                = (== I) . direction
-- runPattern (AtomP a)              = (==) a
-- runPattern (AtomPortP (a,p))      = \(a',p') -> runPattern a a' && runPattern p p'
-- runPattern (EdgeP (Edge apA apB)) = \(Edge apA' apB') -> runPattern apA apA' && runPattern apB apB'

-- matchEdge :: Pattern (Edge (Atom,PortType)) -> Graph -> [Edge (NodeRef)]
-- matchEdge ep g
--   = filter (\e -> runPattern ep (toAtomEdge e g))
--   $ IntMap.elems
--   $ graphEdges g

-- mkEdgePattern :: Edge (Pattern Atom,Pattern PortType) -> EdgePattern
-- mkEdgePattern (Edge (a1,pt1) (a2,pt2)) = EdgeP $ Edge (AtomPortP (a1, pt1)) (AtomPortP (a2, pt2))

-- betaPattern :: EdgePattern
-- betaPattern = mkEdgePattern $ Edge (AtomP A, PortP LI) (AtomP L, PortP RO) 

-- combPattern :: EdgePattern
-- combPattern = mkEdgePattern $ Edge (AtomP ARROW, InPortP) (AnyP, OutPortP)

-- --   ( Pattern(..)
-- --   , match
-- --   , anyNode, atomOf, nodeOf, conn
-- --   )
-- --   where

-- -- import Control.Applicative
-- -- import Data.Maybe
-- -- import Chemlambda.Core.Port
-- -- import Chemlambda.Core.Atom
-- -- import Chemlambda.Core.Node
-- -- import Chemlambda.Core.Graph


-- -- -- | A @Pattern@ is a function that finds things that match a pattern in a graph
-- -- newtype Pattern a b = Pattern {runPattern :: Graph a -> [b]}

-- -- instance Functor (Pattern a) where
-- --   fmap f p = Pattern $ \graph -> map f $ match p graph

-- -- instance Applicative (Pattern a) where
-- --   pure a  = Pattern $ const [a]
-- --   f <*> p = Pattern $ \graph ->
-- --     do
-- --       f' <- match f graph
-- --       p' <- match p graph
-- --       return $ f' p'

-- -- instance Alternative (Pattern a) where
-- --   empty = Pattern (pure [])
-- --   patternA <|> patternB = Pattern $ \graph -> match patternA graph ++ match patternB graph

-- -- instance Monad (Pattern a) where
-- --   return  = pure
-- --   p >>= f = Pattern $ \graph -> concatMap (\res -> match (f res) graph) $ match p graph


-- -- -- | Matches a @Pattern@ on a Graph
-- -- match :: Pattern a b -> Graph a -> [b]
-- -- match pattern graph = runPattern pattern graph


-- -- -- | @anyNode@ returns all the nodes in a graph
-- -- anyNode :: Pattern a (Node a)
-- -- anyNode = Pattern nodes

-- -- -- | @atomOf@ matches nodes with a certain 'Atom'
-- -- atomOf :: Atom -> Pattern a (Node a)
-- -- atomOf a = Pattern $ filter (\node -> a == atom node) . nodes

-- -- -- | @nodeOf@ matches equivalent nodes
-- -- nodeOf :: Eq a => Node a -> Pattern a (Node a)
-- -- nodeOf n = Pattern $ filter (== n) . nodes

-- -- -- | @conn@ matches on a connection between the nodes resulting from two patterns
-- -- -- Each result is put into a graph
-- -- conn
-- --   :: Eq a
-- --   => Pattern a (Node a)
-- --   -> Pattern a (Node a)
-- --   -> [PortSel a]
-- --   -> [PortSel a]
-- --   -> Pattern a (Graph a)
-- -- conn patternA patternB portsA portsB = Pattern $ \graph ->
-- --   let
-- --     portPairs = do
-- --       a <- portsA
-- --       b <- portsB
-- --       return (a,b)

-- --     connectsAtPorts nodeA nodeB portP portQ graph =
-- --       (connects <$> portP nodeA <*> portQ nodeB) == (Just True)

-- --     matchPairs = do
-- --       node     <- match patternA graph
-- --       connNode <- match patternB $ mkGraph $ connections node graph
-- --       return (node, connNode)

-- --     connGroups = do
-- --       (a,b) <- matchPairs
-- --       (p,q) <- portPairs
-- --       return (a,b,p,q)

-- --     connectedPairs = filter (\(a,b,p,q) -> connectsAtPorts a b p q graph) connGroups

-- --   in map (\(a,b,p,q) -> mkGraph [a,b]) connectedPairs
