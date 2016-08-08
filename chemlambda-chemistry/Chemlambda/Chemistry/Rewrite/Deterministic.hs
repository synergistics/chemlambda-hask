module Chemlambda.Chemistry.Rewrite.Deterministic
  ( detRewrite )
  where

import Data.List
import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Chemistry.Reaction
import Chemlambda.Chemistry.Enzymes
import Chemlambda.Chemistry.Rewrite.Util  

detRewrite :: (Ord a, Enum a) => Graph a -> Graph a 
detRewrite graph =
  let 
    result = 
      foldl
        (\graph rsite -> 
          reactInGraph rsite graph)
        graph
        (deterministicReactionSites graph enzymeList)
  in runCombCycle result
