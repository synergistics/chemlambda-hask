module Chemlambda.SampleData.Graphs where

import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Reaction


longIdentity = Graph . concat . take 1000 . iterate (map succNode) $ nodes identity
  where
    succNode node = node { ports = map ((+ 15) <$>) $ ports node }

identity = Graph
  [ lam 4 4 5
  , app 5 2 9
  , foe 9 10 11
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

y = Graph
  [ fo  0 1 2
  , lam 3 4 5
  , fo  4 6 7
  , app 6 7 8
  , app 1 8 3
  , lam 10 11 12
  , fo  11 13 14
  , app 13 14 15
  , app 2 15 10
  , app 5 12 99
  , frout 99
  ]

meh = Graph
  [ lam 0 1 2
  , t   1 
  , lam 2 0 3 
  , app 3 6 5
  , lam 4 4 6
  ]

skk = Graph
  [ foe 3 1 2
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
