module Chemlambda.Chemistry.Rewrite.Random 
  ( randRewrite )
  where

import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Chemistry.Reaction
import Chemlambda.Chemistry.Enzymes
import Chemlambda.Chemistry.Rewrite.Util  


randRewrite :: (Ord a, Enum a) => Graph [Node a] -> IO (Graph [Node a])
randRewrite graph =
  do
    sites <- randomReactionSites graph enzymeList    
    let
      result = 
        foldl
          (\graph r -> 
            reactInGraph r graph)
          graph
          sites  
    return $ runCombCycle result
