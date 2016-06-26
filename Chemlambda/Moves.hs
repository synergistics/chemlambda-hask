{-# LANGUAGE ViewPatterns #-}

module Chemlambda.Moves where

import qualified Chemlambda.Graph as CG
import Chemlambda.Graph hiding (Graph, View, Context) 
import Chemlambda.Patterns
import Chemlambda.Node
import qualified Util.Graph as G
import Util.Graph
import Util.Pattern


type Move = CG.Graph -> CG.Graph


beta :: Move 
beta (matchOn (nodeOf L <&&> hasOf ro A) -> Just lamNode@(c G.:& g)) = 
  let
    Just appNode = ro lamNode 
    
    lamCtx = ctx lamNode
    appCtx = ctx appNode

    Just res1 = arrow <$> (miRef lamNode) <*> (moRef appNode)
    Just res2 = arrow <$> (riRef appNode) <*> (loRef lamNode)  

    result = [res1, res2] 
    newGraph = removeMult [lamCtx, appCtx] g ++ result 
  in
    newGraph 
beta g = g -- if no match was found 


fanIn :: Move
fanIn (matchOn (nodeOf FI <&&> hasOf mo FOE) -> Just fiNode@(c G.:& g)) =
  let
    Just foeNode = mo fiNode 
    
    fiCtx  = ctx fiNode
    foeCtx = ctx foeNode

    Just res1 = arrow <$> (liRef fiNode) <*> (roRef foeNode)
    Just res2 = arrow <$> (riRef fiNode) <*> (loRef foeNode)  

    result = [res1, res2] 
    newGraph = removeMult [fiCtx, foeCtx] g ++ result 
  in
    newGraph 
fanIn g = g

-- comb' :: (View -> Maybe View) -> (View -> Maybe Int) -> Move
-- comb' t tRef (matchOn (hasOf t Arrow) -> Just matchNode@(c G.& g)) =
--   let
--     Just arrowNode = t matchNode
--     Just newRef = moRef arrowNode

--     result = [Context [] (node c) []]
--     newGraph = removeMult [matchNode, arrowNode] g ++ result
--   in
--     newGraph

comb :: Move
comb (matchOn (hasOf mo Arrow) -> Just someNode@(c G.:& g)) =
  let
    Just arrowNode = mo someNode

    someCtx  = ctx someNode
    arrowCtx = ctx arrowNode

    Just newRef = moRef arrowNode

    result = [Context (inEdges c) (node c) [newRef]]
    newGraph = removeMult [someCtx, arrowCtx] g ++ result
  in
    newGraph
comb (matchOn (hasOf lo Arrow) -> Just someNode@(c G.:& g)) =
  let
    Just arrowNode = lo someNode

    someCtx  = ctx someNode
    arrowCtx = ctx arrowNode

    Just newRef = moRef arrowNode

    result = [Context (inEdges c) (node c) (newRef : [outEdges c !! 1])]
    newGraph = removeMult [someCtx, arrowCtx] g ++ result
  in
    newGraph
comb (matchOn (hasOf ro Arrow) -> Just someNode@(c G.:& g)) =
  let
    Just arrowNode = ro someNode

    someCtx  = ctx someNode
    arrowCtx = ctx arrowNode

    Just newRef = moRef arrowNode

    result = [Context (inEdges c) (node c) (outEdges c !! 0 : [newRef])]
    newGraph = removeMult [someCtx, arrowCtx] g ++ result
  in
    newGraph


