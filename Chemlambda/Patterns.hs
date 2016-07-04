module Chemlambda.Patterns where

import Data.List

import qualified Chemlambda.Graph as C
import Chemlambda.Selectors
import qualified Util.Graph as G
import qualified Util.Pattern as P

import Chemlambda.Node
import Util.Graph (node,ctx)


type Pattern = P.Pattern C.View

-- Matches a pattern against a C.Graph, returning possibly a view of the graph from the match
matchOn :: Pattern -> C.Graph -> Maybe C.View
matchOn (P.Pattern p) g = find p (G.toViews g) 

-- Matches on a view who's context has a node at relative position e of v 
-- So hasOf mo L matches on a view that has a middle out node of L 
hasOf :: (C.View -> Maybe C.View) -> Node -> Pattern 
hasOf p n = P.Pattern $ (\v -> ((node . ctx) <$> (p v)) == Just n)

-- Matches on a View that has a node in its context of n
nodeOf :: Node -> Pattern
nodeOf n = P.Pattern $ (\v -> (node . ctx) v == n)

-- Like hasOf, but matches specifically if the p1 and p2 connect
connectsAt :: (C.View -> Maybe C.View) -> (C.View -> Maybe C.View) -> Node -> Pattern
connectsAt p1 p2 n = P.Pattern (\v -> 
  let 
    outgoingView = p v
  in 
    case outgoingView of 
      Just v' -> ((node . ctx) v' == n) && (p2 v' == Just v)
      Nothing -> False 
  

