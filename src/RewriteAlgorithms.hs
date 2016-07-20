module RewriteAlgorithms where

import Data.List
import Port
import Node
import Graph
import Pattern
import Moves
import Reaction


rewrite 
  :: (Ord a, Enum a)
  => [[Enzyme a]]
  -> Graph [Node a]
  -> Graph [Node a]
rewrite ess graph = 
  -- let 
  --   runCombCycle graph = 
  --     let
  --       comb graph = runEnzyme combEnzyme graph

  --       go (Graph []) curr = go curr (comb curr)
  --       go prev curr = 
  --         if prev == curr
  --           then curr
  --           else go curr (comb curr)
  --     in
  --       go (Graph []) graph

  --   result = foldl
  --              (\graph' e -> runEnzyme e graph')
  --              graph
  --              (concat ess)

  -- in runCombCycle result
  let 
    runCombCycle graph = 
      let
        comb graph = runEnzyme combEnzyme graph

        go (Graph []) curr = go curr (comb curr)
        go prev curr = 
          if prev == curr
            then curr
            else go curr (comb curr)
      in
        go (Graph []) graph

    result = 
      foldl 
        (\graph' es ->
          let
            rss = concatMap (\enzyme -> reactionSites enzyme graph') es
          in
            foldl (\graph'' rs -> reactInGraph rs graph'') graph' rss)
        graph
        ess
  in
    runCombCycle result
