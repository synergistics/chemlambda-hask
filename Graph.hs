module Graph where

import Atom


data Graph a = Graph { graph :: [Atom a] } deriving ( Show, Eq )
