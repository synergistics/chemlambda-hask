module Graph where

newtype Graph a = Graph { graph :: [a] } deriving ( Show, Eq )
