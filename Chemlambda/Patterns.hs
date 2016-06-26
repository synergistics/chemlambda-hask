module Chemlambda.Patterns where

import Data.List

import qualified Chemlambda.Graph as C
import Chemlambda.Graph (mo, mi, lo, li, ro, ri)
import qualified Util.Graph as U
import qualified Util.Pattern as P

import Chemlambda.Node
import Util.Graph (node,ctx)


type Pattern = P.Pattern C.View


matchOn :: Pattern -> C.Graph -> Maybe C.View
matchOn (P.Pattern p) g = find p (U.toViews g) 


{- Matches on a view who's context has a node at relative position e of v 
   So hasOf mo L matches on a node that has a middle out of L -}
hasOf :: (C.View -> Maybe C.View) -> Node -> Pattern 
hasOf e n = P.Pattern $ (\v -> ((node . ctx) <$> (e v)) == Just n)

nodeOf :: Node -> Pattern
nodeOf n = P.Pattern $ (\v -> (node . ctx) v == n)
