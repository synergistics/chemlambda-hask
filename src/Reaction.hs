module Reaction where

-- Test performance differences with reacting automatically versus
-- going through the intermediate step of ReactionSite
import Data.List
import Port
import Node
import Graph
import Pattern
import Moves


data Enzyme a = Enzyme
  { pattern :: Pattern [Node a] (Graph [Node a]) 
  , move    :: Graph [Node a] -> Graph [Node (NewId a)] }

data ReactionSite a = ReactionSite 
  { site     :: Graph [Node a]
  , reaction :: Graph [Node a] -> Graph [Node (NewId a)] } 


reactionSites :: Enzyme a -> Graph [Node a] -> [ReactionSite a] 
reactionSites enzyme graph =
  let matches = match (pattern enzyme) graph
  in map (\graph -> ReactionSite graph (move enzyme)) matches

reactionSitesMult :: [Enzyme a] -> Graph [Node a] -> [ReactionSite a]
reactionSitesMult enzymes graph =
  concatMap (\enzyme -> reactionSites enzyme graph) enzymes

runEnzyme
  :: (Ord a, Enum a)
  => Enzyme a
  -> Graph [Node a]
  -> Graph [Node a]
runEnzyme enzyme graph =
  let
    rs = reactionSites enzyme graph
  in foldl' (flip reactInGraph) graph rs

runReaction :: ReactionSite a -> Graph [Node (NewId a)]
runReaction rs = reaction rs $ site rs

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


betaEnzyme :: Eq a => Enzyme a
betaEnzyme = Enzyme betaPattern betaMove

fanInEnzyme :: Eq a => Enzyme a
fanInEnzyme = Enzyme fanInPattern fanInMove

distAEnzyme :: Eq a => Enzyme a
distAEnzyme = Enzyme distAPattern distAMove

distLEnzyme :: Eq a => Enzyme a
distLEnzyme = Enzyme distLPattern distLMove

distFOEnzyme :: Eq a => Enzyme a
distFOEnzyme = Enzyme distFOPattern distFOMove

distFIEnzyme :: Eq a => Enzyme a
distFIEnzyme = Enzyme distFIPattern distFIMove

pruneEnzyme :: Eq a => Enzyme a
pruneEnzyme = Enzyme prunePatterns pruneMove

combEnzyme :: Eq a => Enzyme a
combEnzyme = Enzyme combPattern combMove
