module Chemlambda.Core.Reaction
  ( Enzyme(..)
  , ReactionSite(..)
  , runReaction
  , reactionSites
  , reactInGraph
  , orderedReactionSites
  )
  where

-- Test performance differences with reacting automatically (lazily so not really) versus
-- going through the intermediate step of ReactionSite
import Data.List
import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern


data Enzyme a = Enzyme
  { pattern :: Pattern [Node a] (Graph [Node a]) 
  , move    :: Graph [Node a] -> Graph [Node (NewId a)] }

data ReactionSite a = ReactionSite 
  { site     :: Graph [Node a]
  , reaction :: Graph [Node a] -> Graph [Node (NewId a)] } 

runReaction :: ReactionSite a -> Graph [Node (NewId a)]
runReaction rsite = reaction rsite $ site rsite

reactionSites :: Enzyme a -> Graph [Node a] -> [ReactionSite a] 
reactionSites enzyme graph =
  let matches = match (pattern enzyme) graph
  in map (\graph -> ReactionSite graph (move enzyme)) matches

-- reactionSitesMult :: Eq a => [Enzyme a] -> Graph [Node a] -> [ReactionSite a]
-- reactionSitesMult enzymes graph =
--   concatMap (\enzyme -> reactionSites enzyme graph) enzymes

-- Decent
-- runEnzyme
--   :: (Ord a, Enum a)
--   => Enzyme a
--   -> Graph [Node a]
--   -> Graph [Node a]
-- runEnzyme enzyme graph =
--   let
--     rsites = reactionSites enzyme graph
--   in foldl' (flip reactInGraph) graph rsites

reactInGraph 
  :: (Enum a, Ord a) 
  => ReactionSite a 
  -> Graph [Node a] 
  -> Graph [Node a]
reactInGraph rsite graph =
  let
    withAdded   = graph `plusNew` runReaction rsite
    withRemoved = withAdded `minus` site rsite
  in withRemoved

orderedReactionSites :: Eq a => Graph [Node a] -> [[Enzyme a]] -> [ReactionSite a]
orderedReactionSites graph lolEnzymes = snd $ foldl'
-- (list of lists of enzymes)
  (\(graph, rsites) enzymes ->
    let
      rsites' = concatMap (\enzyme -> reactionSites enzyme graph) enzymes
      sites   = map site rsites'
      graph'  = foldl' minus graph sites
    in (graph', rsites' ++ rsites))
  (graph, [])
  lolEnzymes
