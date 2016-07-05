module Pattern where

import Data.List

import Atom
import Graph
import View


-- (<&&>) :: Pattern a Bool -> Pattern a Bool -> Pattern a Bool
-- (Pattern p) <&&> (Pattern q) = Pattern ((&&) <$> p <*> q)

-- (<||>) :: Pattern a Bool -> Pattern a Bool -> Pattern a Bool
-- (Pattern p) <||> (Pattern q) = Pattern ((||) <$> p <*> q)

data Pattern a b = Pattern (a -> b)

match :: Eq a => Pattern (View a) Bool -> Graph a -> [View a] 
match (Pattern p) g =
  let 
    vs = views g
  in
    filter p vs

many :: Eq a => Pattern a b -> Pattern [a] [b] 
many (Pattern a) = Pattern $ map a 


atom :: (Atom a -> Bool) -> Pattern (View (Atom a)) (Maybe (View (Atom a))) 
atom pred = Pattern $ \v@(a :& g) -> 
  if pred a
    then Just v
    else Nothing



-- atomOf FO_    = Pattern (\(Graph g) -> map isFO g)
-- atomOf FOE_   = Pattern (\(Graph g) -> map isFOE g)
-- atomOf A_     = Pattern (\(Graph g) -> map isA g)
-- atomOf FI_    = Pattern (\(Graph g) -> map isFI g)
-- atomOf ARROW_ = Pattern (\(Graph g) -> map isARROW g)
-- atomOf FRIN_  = Pattern (\(Graph g) -> map isFRIN g)
-- atomOf FROUT_ = Pattern (\(Graph g) -> map isFROUT g)
-- atomOf T_     = Pattern (\(Graph g) -> map isT g)

