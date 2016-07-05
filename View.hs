module View where

import Data.List

import Port
import Atom
import Graph


data View a = a :& (Graph a) deriving ( Show )

mkView :: Eq a => a -> Graph a -> View a
mkView a (Graph g) = a :& (Graph $ filter (/= a) g)

li :: View (Atom a) -> Maybe (View (Atom a))
li (a :& g) = 
  let
    port = liPort a
    port >>= \p' -> find (elem p' . outPorts) g
