{-# LANGUAGE ViewPatterns #-}
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
import Chemlambda.Chemistry.Reaction
import Chemlambda.Chemistry.Enzymes

runCombCycle :: (Enum a, Ord a) => Graph a -> Graph a
runCombCycle graph = 
  let
    comb graph = 
      let a = reactionSites combEnzyme graph
      in case a of
        [] -> graph
        _  -> reactInGraph (head a) graph

    go (nodes -> []) curr = go curr (comb curr)
    go prev curr = 
      if prev == curr
        then curr
        else go curr (comb curr)
  in
    go (mkGraph []) graph

rewriteIO :: (Graph a -> IO (Graph a)) -> IO (Graph a) -> IO (Graph a)
rewriteIO rewrite ioG = do
  g <- ioG
  rewrite g

rewriteIter   rewrite times graph = iterate rewrite graph !! times
rewriteIterIO rewrite times graph = iterate (rewriteIO rewrite) (return graph) !! times
