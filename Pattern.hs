module Pattern where

import Data.List

import Atom
import Graph
import View

import SymbolicAtom

-- (<&&>) :: Pattern a Bool -> Pattern a Bool -> Pattern a Bool
-- (Pattern p) <&&> (Pattern q) = Pattern ((&&) <$> p <*> q)

-- (<||>) :: Pattern a Bool -> Pattern a Bool -> Pattern a Bool
-- (Pattern p) <||> (Pattern q) = Pattern ((||) <$> p <*> q)

data Pattern a b = Pattern (View a -> b)

one :: (Eq a) => Pattern a Bool -> Graph a -> Maybe (View a)
one (Pattern p) g = find p $ views g

many :: (Eq a) => Pattern a Bool -> Graph a -> [(View a)]
many (Pattern p) g = filter p $ views g 

atom :: SAtom -> Pattern a Bool
atom sa = Pattern $ \v@(a :& g) -> case sa of
  L_     -> isL a
  FO_    -> isFO a
  FOE_   -> isFOE a
  A_     -> isA a
  FI_    -> isFI a
  ARROW_ -> isARROW a
  FRIN_  -> isFRIN a
  FROUT_ -> isFROUT a
  T_     -> isT a

-- Matches on two ports that hav
conn :: SAtom -> SAtom -> (Atom a -> Maybe a) -> (Atom a -> Maybe a) -> Pattern a 
conn a b oa ib = 
