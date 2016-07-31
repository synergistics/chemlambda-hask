module Chemlambda.Chemistry.Enzymes
  ( enzymeList
  , deterministicEnzymeList
  , betaEnzyme
  , combEnzyme
  , fanInEnzyme
  , distLEnzyme
  , distAEnzyme
  , distFIEnzyme
  , distFOEnzyme
  , pruneAEnzyme
  , pruneFIEnzyme
  , pruneLEnzyme
  , pruneFObEnzyme
  , pruneFOEbEnzyme
  , pruneFOcEnzyme
  , pruneFOEcEnzyme
  )
  where

import Chemlambda.Chemistry.Reaction
import Chemlambda.Chemistry.Patterns
import Chemlambda.Chemistry.Moves


enzymeList :: Eq a => [Enzyme a]
enzymeList = 
  [ distFOEnzyme
  , distAEnzyme
  , distLEnzyme
  , distFIEnzyme
  , betaEnzyme
  , fanInEnzyme
  , pruneAEnzyme
  , pruneLEnzyme
  , pruneFIEnzyme
  , pruneFObEnzyme
  , pruneFOEbEnzyme
  , pruneFOcEnzyme
  , pruneFOEcEnzyme
  ]

deterministicEnzymeList :: Eq a => [[Enzyme a]]
deterministicEnzymeList =
  [ [ distFOEnzyme ]
  , [ distAEnzyme, distLEnzyme, distFIEnzyme ]
  , [ betaEnzyme, fanInEnzyme ]
  , [ pruneAEnzyme, pruneLEnzyme, pruneFIEnzyme ]
  , [ pruneFObEnzyme, pruneFOEbEnzyme ]
  , [ pruneFOcEnzyme, pruneFOEcEnzyme ]
  ]


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
