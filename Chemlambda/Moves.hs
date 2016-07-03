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

    lamCtx = ctx lamNode
    foCtx  = ctx foNode
    
    Just edgeA = miRef lamNode 
    Just edgeB = loRef lamNode
    Just edgeC = roRef lamNode
    Just edgeD = loRef foNode
    Just edgeE = roRef foNode
    edgeK = unusedEdges !! 0
    edgeL = unusedEdges !! 1
    edgeI = unusedEdges !! 2
    edgeJ = unusedEdges !! 3

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


distFI :: Move
distFI (matchOn ((nodeOf FI) <&&> 
                 (hasOf mo FO)) -> Just fiNode@(c G.:& g)) = 
  let
    unusedEdges = [0..] \\ edges g

    Just foNode = mo fiNode

    fiCtx = ctx fiNode
    foCtx = ctx foNode

    Just edgeA = liRef fiNode
    Just edgeB = riRef fiNode
    Just edgeC = moRef fiNode
    Just edgeD = loRef foNode
    Just edgeE = roRef foNode
    edgeK = unusedEdges !! 0
    edgeL = unusedEdges !! 1
    edgeI = unusedEdges !! 2
    edgeJ = unusedEdges !! 3

    result = [ fo edgeA edgeI edgeJ 
             , fo edgeB edgeK edgeL
             , fi edgeI edgeK edgeD
             , fi edgeJ edgeL edgeE
             ]

    newGraph = removeCtxs [fiCtx, foCtx] g ++ result
  in
    newGraph
distFI g = g


distFO :: Move
distFO (matchOn ((nodeOf FO) <&&>
                 (hasOf ro FOE)) -> Just foNode@(c G.:& g)) =
  let
    unusedEdges = [0..] \\ edges g

    Just foeNode = ro foNode

    foCtx  = ctx foNode
    foeCtx = ctx foeNode

    Just edgeA = miRef foNode
    Just edgeB = loRef foNode
    Just edgeC = roRef foNode
    Just edgeD = loRef foeNode
    Just edgeE = roRef foeNode
    edgeK = unusedEdges !! 0
    edgeL = unusedEdges !! 1
    edgeI = unusedEdges !! 2
    edgeJ = unusedEdges !! 3

    result = [ fi  edgeI edgeJ edgeB
             , fo  edgeK edgeI edgeD 
             , fo  edgeL edgeJ edgeE 
             , foe edgeA edgeK edgeL 
             ]

    newGraph = removeCtxs [foCtx, foeCtx] g ++ result
  in
    newGraph
distFO g = g


pruneA :: Move
pruneA (matchOn ((nodeOf A)  <&&> 
                 (hasOf mo T)) -> Just someNode@(c G.:& g)) = 
  let
    Just terNode = mo someNode

    someCtx = ctx someNode
    terCtx  = ctx terNode 
    
    Just edgeA = liRef someNode
    Just edgeB = riRef someNode

    result = [ ter edgeA
             , ter edgeB
             ]
    
    newGraph = removeCtxs [someCtx, terCtx] g ++ result
  in
    newGraph
pruneA g = g 


pruneFI :: Move
pruneFI (matchOn ((nodeOf FI) <&&> 
                 (hasOf mo T)) -> Just someNode@(c G.:& g)) = 
  let
    Just terNode = mo someNode

    someCtx = ctx someNode
    terCtx  = ctx terNode 
    
    Just edgeA = liRef someNode
    Just edgeB = riRef someNode

    result = [ ter edgeA
             , ter edgeB
             ]
    
    newGraph = removeCtxs [someCtx, terCtx] g ++ result
  in
    newGraph
pruneFI g = g 


pruneL :: Move
pruneL (matchOn ((nodeOf L) <&&>
                 (hasOf ro T)) -> Just lamNode@(c G.:& g)) = 
  let
    Just terNode = ro lamNode

    lamCtx = ctx lamNode
    terCtx = ctx terNode

    Just edgeA = miRef lamNode
    Just edgeB = loRef lamNode

    result = [ ter edgeA
             , frin edgeB
             ]
    
    newGraph = removeCtxs [lamCtx, terCtx] g ++ result
  in
    newGraph
pruneL g = g 


-- Prune FO lo matches when the lo is terminated 
pruneFO_lo :: Move
pruneFO_lo (matchOn (((nodeOf FO) <||> (nodeOf FOE)) <&&>
                     (hasOf lo T)) -> Just foNode@(c G.:& g)) = 
  let
    Just terNode = lo foNode

    foCtx  = ctx foNode
    terCtx = ctx terNode

    Just edgeA = miRef foNode
    Just edgeC = roRef foNode

    result = [ arrow edgeA edgeC ]
    
    newGraph = removeCtxs [foCtx, terCtx] g ++ result
  in
    newGraph
pruneFO_lo g = g 


pruneFO_ro :: Move
pruneFO_ro (matchOn (((nodeOf FO) <||> (nodeOf FOE)) <&&>
                     (hasOf ro T)) -> Just foNode@(c G.:& g)) = 
  let
    Just terNode = ro foNode

    foCtx  = ctx foNode
    terCtx = ctx terNode

    Just edgeA = miRef foNode
    Just edgeB = loRef foNode

    result = [ arrow edgeA edgeB ]
    
    newGraph = removeCtxs [foCtx, terCtx] g ++ result
  in
    newGraph
pruneFO_ro g = g 



