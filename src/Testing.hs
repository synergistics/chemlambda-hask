module Testing where

import Port
import Atom
import Node
import Graph
import Pattern
import Moves
import GraphMod
import Reduction


reduce1 :: (Ord a, Enum a) => Graph a -> Graph a
reduce1 graph = reduceGeneral standardPatternMoveList (const True) graph


reduce2 :: (Ord a, Enum a) => Graph a -> Graph a
reduce2 graph = combCycle $ reduce1 graph

comb :: (Ord a, Enum a) => Graph a -> Graph a
comb graph =
  let
    mods = graphMods (combPattern, combMove) graph
  in
    foldr applyGraphMod graph mods


combCycle :: (Ord a, Enum a) => Graph a -> Graph a 
combCycle = combCycle' (Graph [])
  where
    combCycle' :: (Ord a, Enum a) => Graph a -> Graph a -> Graph a 
    combCycle' (Graph []) current = combCycle' current $ comb current
    combCycle' last current = 
      if last == current
        then current
        else combCycle' current $ comb current

identity = Graph
  [ lam 1 1 2
  , lam 4 4 5
  , app 2 5 9
  , frout 9
  ]

omega = Graph
  [ lam 0 1 2
  , fo  1 3 4
  , app 3 4 0
  , lam 5 6 7
  , fo  6 8 9
  , app 8 9 5
  , app 2 7 10
  , frout 10
  ]

skk = Graph
  [ fo  3 1 2
  , lam 5 4 3
  , lam 4 6 5
  , t   6
  , app 41 1 51
  , app 51 2 61
  , lam 7 84 41
  , lam 8 85 7
  , lam 9 86 8
  , app 10 11 9
  , app 84 12 10
  , app 85 13 11
  , fo  86 12 13
  ]

quine = Graph
  [ lam 5 1 2
  , fi  1 7 6
  , app 2 3 4
  , fi  4 6 9
  , lam 8 7 10
  , foe 9 5 8
  , foe 10 12 11
  , app 12 15 13
  , foe 13 15 14
  , app 11 14 3
  ]
