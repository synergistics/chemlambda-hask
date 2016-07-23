module Chemlambda.Standard.Rewrite 
  ( rewrite 
  , rewriteCycle
  , priorityBasedReactionSites
  , runCombCycle -- remove this later
  )
  where

import Data.List
import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Core.Reaction
import Chemlambda.Standard.Enzymes


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

priorityBasedReactionSites :: Eq a => Graph [Node a] -> [[Enzyme a]] -> [ReactionSite a]
priorityBasedReactionSites graph ess = snd $ foldl'
  (\(graph, rs) es ->
    let
      rs'    = reactionSitesMult es graph
      s      = map site rs'
      graph' = foldl' minus graph s 
    in (graph', rs ++ rs'))
  (graph, [])
  ess

rewrite :: (Ord a, Enum a) => Graph [Node a] -> Graph [Node a] 
rewrite graph =
  let 
    result = 
      foldl
        (\graph r -> 
          reactInGraph r graph)
        graph
        (priorityBasedReactionSites graph standardEnzymes)
  in runCombCycle result

rewriteCycle times graph = iterate rewrite graph !! times
  -- let 
  --   result = foldl
  --              (\graph' e -> runEnzyme e graph')
  --              graph
  --              (concat ess)
  -- in runCombCycle result
