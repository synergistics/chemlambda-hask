{-# LANGUAGE PartialTypeSignatures #-}
module RewriteAlgorithms where

import Data.List
import Port
import Node
import Graph
import Pattern
import Moves
import Reaction

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

getReactionSites graph ess = foldl'
  (\(graph, rs) es ->
    let
      rs'    = reactionSitesMult es graph
      s      = map site rs'
      graph' = foldl' minus graph s 
    in (graph', rs ++ rs'))
  (graph, [])
  ess

sites graph = map site . snd . getReactionSites graph


standardEnzymes :: Eq a => [[Enzyme a]]
standardEnzymes =
  [ [ distFOEnzyme ]
  , [ distAEnzyme, distLEnzyme, distFIEnzyme ]
  , [ betaEnzyme, fanInEnzyme ]
  , [ pruneEnzyme ]
  ]

rewrite 
  :: (Ord a, Enum a)
  => [[Enzyme a]]
  -> Graph [Node a]
  -> Graph [Node a] 
  -- -> Graph [Node a]
rewrite ess graph = 
  -- let 
  --   result = foldl
  --              (\graph' e -> runEnzyme e graph')
  --              graph
  --              (concat ess)
  -- in runCombCycle result

  let 
    result = 
      foldl
        (\graph r -> 
          reactInGraph r graph)
        graph
        (snd $ getReactionSites graph ess)
  in result
rewriteWithComb
  :: (Ord a, Enum a)
  => [[Enzyme a]]
  -> Graph [Node a]
  -> Graph [Node a] 
rewriteWithComb enzymes = runCombCycle . rewrite enzymes 
  -- in map site rs 
    -- result = 
    --   foldl 
    --     (\graph' es ->
    --       let
    --         rss = concatMap (\enzyme -> reactionSites enzyme graph') es
    --       in
    --         foldl (\graph'' rs -> reactInGraph rs graph'') graph' rss)
    --     graph
    --     ess
    -- runCombCycle result
