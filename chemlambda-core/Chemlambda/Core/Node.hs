module Chemlambda.Core.Node 
  ( Node(..)
  , PortSel
  , lam, fo, foe, app, fi, arrow, frin, frout, t 
  , liPort, riPort, miPort, loPort, roPort, moPort 
  , connects
  , hasPortId
  )
  where

import Data.List
import Chemlambda.Core.Connectable
import Chemlambda.Core.Atom
import Chemlambda.Core.Port


-- | A Chemlambda node
-- A @Node@ is a combination of an 'Atom' and a list of 'Port's
data Node a = Node
  { atom  :: Atom
  , ports :: [Port a] }
  deriving Eq

instance Show a => Show (Node a) where
  show (Node a ps) = "Node " ++ show a ++ " " ++ show ps

-- | Two @Node@s connect via a connection at one or more of their ports
instance Eq a => Connectable (Node a) where
  connects m n = 
    let
      shared = intersectBy (\p q -> connects p q) (ports m) (ports n)
    in
      not $ null shared


hasPortId :: Eq a => Node a -> a -> Bool
hasPortId node id = elem id $ (map portId . ports) node


-- === Elementary molecule (defined by the Chemlambda specification) constructors
lam :: a -> a -> a -> Node a
lam a b c = Node L [Mi a, Lo b, Ro c]

fo :: a -> a -> a -> Node a
fo a b c = Node FO [Mi a, Lo b, Ro c]

foe :: a -> a -> a -> Node a
foe a b c = Node FOE [Mi a, Lo b, Ro c]

app :: a -> a -> a -> Node a
app a b c = Node A [Li a, Ri b, Mo c]

fi :: a -> a -> a -> Node a
fi a b c = Node FI [Li a, Ri b, Mo c]

arrow :: a -> a -> Node a
arrow a b = Node ARROW [Mi a, Mo b]

frin :: a -> Node a
frin a = Node FRIN [Mo a]

frout :: a -> Node a
frout a = Node FROUT [Mi a]

t :: a -> Node a
t a = Node T [Mi a]


-- | A @PortSel@ selects a port in a 'Node'
type PortSel a = Node a -> Maybe (Port a)

-- | @mkPortSel@ takes a port constructor predicate
mkPortSel :: (Port a -> Bool) -> PortSel a
mkPortSel isPort = find isPort . ports


-- === Port selectors
liPort :: PortSel a
liPort = mkPortSel isLi

riPort :: PortSel a
riPort = mkPortSel isRi

miPort :: PortSel a
miPort = mkPortSel isMi

loPort :: PortSel a
loPort = mkPortSel isLo

roPort :: PortSel a
roPort = mkPortSel isRo

moPort :: PortSel a
moPort = mkPortSel isMo
