{-# LANGUAGE ViewPatterns #-}

module Chemlambda.Moves where

import Data.List

import qualified Chemlambda.Graph as C
import Chemlambda.Selectors 
import Chemlambda.Patterns
import Chemlambda.Node
import qualified Util.Graph as G
import Util.Graph
import Util.Pattern


type Move = C.Graph -> C.Graph


beta :: Move 
beta (matchOn (nodeOf L <&&> hasOf ro A) -> Just lamNode@(c G.:& g)) = 
  let
    Just appNode = ro lamNode 
    
    lamCtx = ctx lamNode
    appCtx = ctx appNode

    Just res1 = arrow <$> (miRef lamNode) <*> (moRef appNode)
    Just res2 = arrow <$> (riRef appNode) <*> (loRef lamNode)  

    result = [res1, res2] 
    newGraph = removeCtxs [lamCtx, appCtx] g ++ result 
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
    newGraph = removeCtxs [fiCtx, foeCtx] g ++ result 
  in
    newGraph 
fanIn g = g


-- Reduce the repetition on this one
comb :: Move
comb (matchOn (hasOf mo Arrow) -> Just someNode@(c G.:& g)) =
  let
    Just arrowNode = mo someNode

    someCtx  = ctx someNode
    arrowCtx = ctx arrowNode

    Just newRef = moRef arrowNode

    result = [Context (inEdges c) (node c) [newRef]]
    newGraph = removeCtxs [someCtx, arrowCtx] g ++ result
  in
    newGraph
comb (matchOn (hasOf lo Arrow) -> Just someNode@(c G.:& g)) =
  let
    Just arrowNode = lo someNode

    someCtx  = ctx someNode
    arrowCtx = ctx arrowNode

    Just newRef = moRef arrowNode

    result = [Context (inEdges c) (node c) (newRef : [outEdges c !! 1])]
    newGraph = removeCtxs [someCtx, arrowCtx] g ++ result
  in
    newGraph
comb (matchOn (hasOf ro Arrow) -> Just someNode@(c G.:& g)) =
  let
    Just arrowNode = ro someNode

    someCtx  = ctx someNode
    arrowCtx = ctx arrowNode

    Just newRef = moRef arrowNode

    result = [Context (inEdges c) (node c) (outEdges c !! 0 : [newRef])]
    newGraph = removeCtxs [someCtx, arrowCtx] g ++ result
  in
    newGraph
comb g = g

distL :: Move
distL (matchOn ((nodeOf L)    <&&> 
                (hasOf ro FO) <||>
                (hasOf ro FOE)) -> Just lamNode@(c G.:& g)) = 
  let
    unusedEdges = [0..] \\ edges g
    -- FO or FOE
    Just foNode = ro lamNode

    Just edgeA = miRef lamNode 
    Just edgeB = loRef lamNode
    Just edgeC = roRef lamNode
    Just edgeD = loRef foNode
    Just edgeE = roRef foNode
    edgeK = unusedEdges !! 0
    edgeL = unusedEdges !! 1
    edgeI = unusedEdges !! 2
    edgeJ = unusedEdges !! 3


    lamCtx = ctx lamNode
    foCtx  = ctx foNode
    
    result = [ foe edgeA edgeK edgeL 
             , lam edgeK edgeI edgeD
             , lam edgeL edgeJ edgeE
             , fi  edgeI edgeJ edgeB
             ]
    newGraph = removeCtxs [lamCtx, foCtx] g ++ result
  in
    newGraph
distL g = g


distA :: Move
distA (matchOn ((nodeOf A)    <&&>
                (hasOf mo FO) <||>
                (hasOf mo FOE)) -> Just appNode@(c G.:& g)) = 
  let
    unusedEdges = [0..] \\ edges g
    -- FO or FOE
    Just foNode = mo appNode

    Just edgeA = liRef appNode
    Just edgeB = riRef appNode
    Just edgeC = moRef appNode
    Just edgeD = loRef foNode
    Just edgeE = roRef foNode
    edgeK = unusedEdges !! 0
    edgeL = unusedEdges !! 1
    edgeI = unusedEdges !! 2
    edgeJ = unusedEdges !! 3

    
    appCtx = ctx appNode
    foCtx  = ctx foNode
    
    result = [ foe edgeA edgeI edgeJ
             , foe edgeB edgeK edgeL 
             , app edgeI edgeK edgeD
             , app edgeJ edgeL edgeE
             ]

    newGraph = removeCtxs [appCtx, foCtx] g ++ result
  in
    newGraph
