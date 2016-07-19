module RewriteAlgorithms where

import Data.List
import Port
import Node
import Graph
import Pattern
import Moves
import Reaction


rewrite 
  :: (Eq a, Enum a)
  => [Enzyme a]
  -> Graph [Node a]
  -> Graph [Node a]
rewrite es graph = foldl (flip runEnzyme) graph es

