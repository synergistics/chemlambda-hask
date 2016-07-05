module View where

import Data.List

import Port
import Atom
import Graph


data View a = Atom a :& Graph a deriving ( Show )

mkView :: Eq a => Atom a -> Graph a -> View a
mkView a (Graph g) = a :& (Graph $ filter (/= a) g)

views :: Eq a => Graph a -> [View a]
views g = map (\a -> mkView a g) $ graph g

(!&) :: Eq a => Graph a -> Int -> View a
g !& n = mkView (graph g !! n) g


findAtomAtPort :: (Eq a) => 
  (Atom a -> Maybe a) ->
  (Atom a -> [a]) ->
  View a ->
  Maybe (View a)
findAtomAtPort portSel directionSel (a :& g) = 
  let
    port = portSel a
    a'   = port >>= \p' -> find (elem p' . directionSel) (graph g)
  in 
    mkView <$> a' <*> Just (Graph (a : (graph g))) 

li :: (Eq a) => View a -> Maybe (View a)
li = findAtomAtPort liPort outPorts

ri :: (Eq a) => View a -> Maybe (View a)
ri = findAtomAtPort riPort outPorts

mi :: (Eq a) => View a -> Maybe (View a)
mi = findAtomAtPort miPort outPorts

lo :: (Eq a) => View a -> Maybe (View a)
lo = findAtomAtPort loPort inPorts

ro :: (Eq a) => View a -> Maybe (View a)
ro = findAtomAtPort roPort inPorts

mo :: (Eq a) => View a -> Maybe (View a)
mo = findAtomAtPort moPort inPorts

