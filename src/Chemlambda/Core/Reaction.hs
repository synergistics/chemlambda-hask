module Chemlambda.Core.Reaction where

-- Test performance differences with reacting automatically versus
-- going through the intermediate step of ReactionSite
import Data.List
import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Core.Moves


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

pruneAEnzyme :: Eq a => Enzyme a
pruneAEnzyme = Enzyme pruneAPattern pruneAMove

pruneFIEnzyme :: Eq a => Enzyme a
pruneFIEnzyme = Enzyme pruneFIPattern pruneFIMove

pruneLEnzyme :: Eq a => Enzyme a
pruneLEnzyme = Enzyme pruneLPattern pruneLMove

pruneFObEnzyme :: Eq a => Enzyme a
pruneFObEnzyme = Enzyme pruneFObPattern pruneFObMove

pruneFOcEnzyme :: Eq a => Enzyme a
pruneFOcEnzyme = Enzyme pruneFOcPattern pruneFOcMove

pruneFOEbEnzyme :: Eq a => Enzyme a
pruneFOEbEnzyme = Enzyme pruneFOEbPattern pruneFOEbMove

pruneFOEcEnzyme :: Eq a => Enzyme a
pruneFOEcEnzyme = Enzyme pruneFOEcPattern pruneFOEcMove

combEnzyme :: Eq a => Enzyme a
combEnzyme = Enzyme combPattern combMove
