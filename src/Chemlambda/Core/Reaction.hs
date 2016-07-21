module Chemlambda.Core.Reaction where

-- Test performance differences with reacting automatically versus
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
runReaction rs = reaction rs $ site rs


reactionSites :: Enzyme a -> Graph [Node a] -> [ReactionSite a] 
reactionSites enzyme graph =
  let matches = match (pattern enzyme) graph
  in map (\graph -> ReactionSite graph (move enzyme)) matches

reactionSitesMult :: Eq a => [Enzyme a] -> Graph [Node a] -> [ReactionSite a]
reactionSitesMult enzymes graph =
  concatMap (\enzyme -> reactionSites enzyme graph) enzymes

-- Decent
runEnzyme
  :: (Ord a, Enum a)
  => Enzyme a
  -> Graph [Node a]
  -> Graph [Node a]
runEnzyme enzyme graph =
  let
    rs = reactionSites enzyme graph
  in foldl' (flip reactInGraph) graph rs


reactInGraph 
  :: (Enum a, Ord a) 
  => ReactionSite a 
  -> Graph [Node a] 
  -> Graph [Node a]
reactInGraph rs graph =
  let
    withAdded   = graph `plusNew` runReaction rs
    withRemoved = withAdded `minus` site rs
  in withRemoved
