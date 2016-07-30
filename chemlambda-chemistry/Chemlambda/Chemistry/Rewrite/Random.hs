module Chemlambda.Chemistry.Rewrite.Random where

import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Core.Reaction
import Chemlambda.Chemistry.Enzymes


runCombCycle :: (Enum a, Ord a) => Graph [Node a] -> Graph [Node a]
runCombCycle graph = 
  let
    comb graph = 
      let a = reactionSites combEnzyme graph
      in case a of
        [] -> graph
        _  -> reactInGraph (head a) graph

    go (Graph []) curr = go curr (comb curr)
    go prev curr = 
      if prev == curr
        then curr
        else go curr (comb curr)
  in
    go (Graph []) graph

rewrite :: (Ord a, Enum a) => Graph [Node a] -> IO (Graph [Node a])
rewrite graph =
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

rewriteCycle times graph = 
  let
    a ioGraph =
      do
        g <- ioGraph
        rewrite g 
    in iterate a (rewrite graph) !! times

