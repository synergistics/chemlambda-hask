module Chemlambda.Experimental.ComonadicGraphs.Reaction where

import Data.List
import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Chemistry.Patterns
import Chemlambda.Chemistry.Moves


data Enzyme a = Enzyme
  { pattern  :: Pattern [Node a] (Graph [Node a])
  , move     :: Graph [Node a] -> Graph [Node (NewId a)]
  , ePriority :: Int }

data ReactionSite a = ReactionSite 
  { site      :: Graph [Node a]
  , reaction  :: Graph [Node a] -> Graph [Node (NewId a)]
  , sPriority :: Int } 


reactionSites :: Enzyme a -> Graph [Node a] -> [ReactionSite a] 
reactionSites enzyme graph =
  let matches = match (pattern enzyme) graph
  in map (\graph' -> ReactionSite graph' (move enzyme) (ePriority enzyme)) matches


betaEnzyme :: Eq a => Enzyme a
betaEnzyme = Enzyme betaPattern betaMove 3 

fanInEnzyme :: Eq a => Enzyme a
fanInEnzyme = Enzyme fanInPattern fanInMove 3

distAEnzyme :: Eq a => Enzyme a
distAEnzyme = Enzyme distAPattern distAMove 4 

distLEnzyme :: Eq a => Enzyme a
distLEnzyme = Enzyme distLPattern distLMove 4 

distFOEnzyme :: Eq a => Enzyme a
distFOEnzyme = Enzyme distFOPattern distFOMove 5

distFIEnzyme :: Eq a => Enzyme a
distFIEnzyme = Enzyme distFIPattern distFIMove 4

pruneAEnzyme :: Eq a => Enzyme a
pruneAEnzyme = Enzyme pruneAPattern pruneAMove 2

pruneFIEnzyme :: Eq a => Enzyme a
pruneFIEnzyme = Enzyme pruneFIPattern pruneFIMove 2

pruneLEnzyme :: Eq a => Enzyme a
pruneLEnzyme = Enzyme pruneLPattern pruneLMove 2

pruneFObEnzyme :: Eq a => Enzyme a
pruneFObEnzyme = Enzyme pruneFObPattern pruneFObMove 1

pruneFOcEnzyme :: Eq a => Enzyme a
pruneFOcEnzyme = Enzyme pruneFOcPattern pruneFOcMove 0

pruneFOEbEnzyme :: Eq a => Enzyme a
pruneFOEbEnzyme = Enzyme pruneFOEbPattern pruneFOEbMove 1

pruneFOEcEnzyme :: Eq a => Enzyme a
pruneFOEcEnzyme = Enzyme pruneFOEcPattern pruneFOEcMove 0

combEnzyme :: Eq a => Enzyme a
combEnzyme = Enzyme combPattern combMove (-1)

