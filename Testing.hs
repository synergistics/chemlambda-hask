module Testing where 

import Atom
import Graph
import View
import Pattern

import SymbolicAtom

g = Graph
  [ frin 1 
  , lam 1 2 3
  , lam 7 8 9
  , app 3 4 0
  , arrow 2 5
  , arrow 5 4
  , frout 4 
  ] 
