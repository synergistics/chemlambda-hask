module Chemlambda.Chemistry.Rewrite.Util 
  ( runCombCycle
  , rewriteIO
  , rewriteIter
  , rewriteIterIO
  ) 
  where

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

rewriteIO :: (Graph [Node a] -> IO (Graph [Node a])) -> IO (Graph [Node a]) -> IO (Graph [Node a])
rewriteIO rewrite ioG = do
  g <- ioG
  rewrite g

rewriteIter   rewrite times graph = iterate rewrite graph !! times
rewriteIterIO rewrite times graph = iterate (rewriteIO rewrite) (return graph) !! times
