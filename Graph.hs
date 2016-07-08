module Graph where

import Node


newtype Graph a = Graph { unGraph :: [Node a] } deriving ( Show, Eq )
