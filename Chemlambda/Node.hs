{-# LANGUAGE ViewPatterns #-}

module Chemlambda.Node where 

import qualified Data.List as L
import Util.Graph

data Node
  = L     
  | A     
  | FI    
  | FO    
  | FOE   
  | Arrow 
  | T     
  | FRIN  
  | FROUT 
  -- | Any -- For pattern matching
  deriving ( Show, Read, Eq )

lam :: Int -> Int -> Int -> Context Node Int
lam mi lo ro                         = Context [mi] L [lo, ro]

app :: Int -> Int -> Int -> Context Node Int
app li ri mo                         = Context [li, ri] A [mo]

fi :: Int -> Int -> Int -> Context Node Int
fi li ri mo                          = Context [li, ri] FI [mo]

fo :: Int -> Int -> Int -> Context Node Int
fo mi lo ro                          = Context [mi] FO [lo, ro]

foe :: Int -> Int -> Int -> Context Node Int
foe mi lo ro                         = Context [mi] FOE [lo, ro]

arrow :: Int -> Int -> Context Node Int
arrow mi mo                          = Context [mi] Arrow [mo]

ter :: Int -> Context Node Int
ter mi                               = Context [mi] T []

frin :: Int -> Context Node Int
frin mo                              = Context [] FRIN [mo]

frout :: Int -> Context Node Int
frout mi                             = Context [mi] FROUT []

-- instance Matching Node where
--   match Any _ = True
--   match _ Any = True
--   match x y   = x == y 

-- instance (Matching e) => Matching [e] where
--   match ms ns =
--     if (length ms) == (length ns)
--       then L.and $ zipWith match ms ns 
--       else False
    
-- -- Convert the edges of a Context to the nodes to which they point 
-- nodeEdges :: (Eq e) => Context Node e -> Graph Node e -> Context Node Node
-- nodeEdges c graph = 
--   let 
--     inNodes  = map node (inContexts c graph) 
--     outNodes = map node (outContexts c graph) 
--   in 
--     Context inNodes (node c) outNodes  

-- -- -- data Node a
-- -- --   = L     a a a  
-- -- --   | A     a a a
-- -- --   | FI    a a a
-- -- --   | FO    a a a
-- -- --   | FOE   a a a
-- -- --   | Arrow a a
-- -- --   | T     a
-- -- --   | FRIN  a
-- -- --   | FROUT a
-- -- --   deriving ( Show, Read, Eq )


-- -- -- Can be used for symbolic matches and actual graph building. Used for pattern matching and concrete implementation.
-- lam :: a -> a -> a -> Context Node a
-- lam mi lo ro                         = Context [mi] L [lo, ro]

-- app :: a -> a -> a -> Context Node a
-- app li ri mo                         = Context [li, ri] A [mo]

-- fi :: a -> a -> a -> Context Node a
-- fi li ri mo                          = Context [li, ri] FI [mo]

-- fo :: a -> a -> a -> Context Node a
-- fo mi lo ro                          = Context [mi] FO [lo, ro]

-- foe :: a -> a -> a -> Context Node a
-- foe mi lo ro                         = Context [mi] FOE [lo, ro]

-- arrow :: a -> a -> Context Node a
-- arrow mi mo                          = Context [mi] Arrow [mo]

-- ter :: a -> Context Node a
-- ter mi                               = Context [mi] T []

-- frin :: a -> Context Node a
-- frin mo                              = Context [] FRIN [mo]

-- frout :: a -> Context Node a
-- frout mi                             = Context [mi] FROUT []


-- --   -- let
-- --   -- -- This is making the perhaps dangerous assumption that contexts with the same nodes are the same. 
-- --   -- -- This would not work in the case of duplicates, but dupes would be illogical anyways.
-- --   --   context = find (\(Context _ n _) -> n == node) graph 
-- --   --   -- newGraph = filter (\c -> return c /= context) graph 
-- --   -- in
-- --   --   (:& graph) <$> context -- Make the context into a view by combining it with the graph. context is a maybe so it must be fmapped. 



-- -- ie: Incoming edges, oe: Outgoing edges
-- matchGraph :: (Eq e) => Context Node Node -> Graph Node e -> Maybe (View Node e) 
-- matchGraph c g =  (\x -> [x] :& g) <$> (L.find (\c' -> match c (nodeEdges c' g)) g)

-- -- ins :: (Eq n, Eq e) => n -> Graph n e -> Maybe [View n e]
-- -- ins n g = 
-- --   let
-- -- --     c :& _ = nodeContext n g
-- -- --     fmap (ins')

-- -- -- abc :: Graph Node -> Bool 
-- -- -- abc (nodeContext L -> Just (Context _ L _ :& _)) = True
-- -- -- abc _ = False


-- testGraph :: CLGraph 
-- testGraph = 
--   [ frin 1 
--   , frout 2
--   , lam 1 2 3 
--   , app 3 4 5 
--   , ter 5
--   , frin 4 ]

