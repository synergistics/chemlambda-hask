module Chemlambda.Experimental.ComonadicGraphs.Graph where

data Atom = L | FO | FOE | A | FI | ARROW | FRIN | FROUT | T deriving ( Show, Eq )
data Node a = Node Atom [a] deriving ( Show, Eq )
data Graph a = Graph { node :: a, nodes :: [Graph a] } deriving ( Show )

class Comonad c where
  extract :: c a -> a
  unfold  :: c a -> c (c a)

instance Comonad Graph where
  extract (Graph n gs) = n

  unfold g@(Graph n gs) = Graph g (map unfold gs)


(!?) :: [a] -> Int -> Maybe a
xs !? i =
  if i > (length xs - 1) then Nothing else Just $ xs !! i

atEdge :: Int -> Graph a -> Maybe (Graph a)
atEdge i (Graph _ gs) = gs !? i


connects :: Eq a => Node a -> Node a -> Bool
connects n@(Node _ es) m@(Node _ ds) = any (`elem` ds) es && n /= m

mkGraph :: Eq a => [Node a] -> Graph Atom 
mkGraph ns =
  let
    n@(Node x es) = head ns
    connectedNodes = filter (connects n) ns
  in Graph x (map mkGraph $ map (\n -> n:filter (/= n) ns) connectedNodes)

omega = 
  [ Node L     [0,1,2]
  , Node FO    [1,3,4]
  , Node A     [3,4,0]
  , Node L     [5,6,7]
  , Node FO    [6,8,9]
  , Node A     [8,9,5]
  , Node A     [2,7,10]
  , Node FROUT [10]
  ]
