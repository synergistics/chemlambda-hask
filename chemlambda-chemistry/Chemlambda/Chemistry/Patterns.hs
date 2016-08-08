module Chemlambda.Chemistry.Patterns 
  ( betaPattern
  , combPattern
  , fanInPattern
  , distLPattern
  , distAPattern
  , distFIPattern
  , distFOPattern
  , pruneAPattern
  , pruneFIPattern
  , pruneLPattern
  , pruneFObPattern
  , pruneFOEbPattern
  , pruneFOcPattern
  , pruneFOEcPattern
  )
  where

import Control.Applicative
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern

betaPattern :: Eq a => Pattern a (Graph a)
betaPattern = conn (atomOf L) (atomOf A) [roPort] [liPort]

combPattern :: Eq a => Pattern a (Graph a)
combPattern = conn anyNode (atomOf ARROW) [loPort,roPort,moPort] [miPort]

fanInPattern :: Eq a => Pattern a (Graph a)
fanInPattern = conn (atomOf FI) (atomOf FOE) [moPort] [miPort]

distLPattern :: Eq a => Pattern a (Graph a)
distLPattern = conn (atomOf L) (atomOf FO <|> atomOf FOE) [roPort] [miPort]

distAPattern :: Eq a => Pattern a (Graph a)
distAPattern = conn (atomOf A) (atomOf FO <|> atomOf FOE) [moPort] [miPort]

distFIPattern :: Eq a => Pattern a (Graph a)
distFIPattern = conn (atomOf FI) (atomOf FO) [moPort] [miPort]

distFOPattern :: Eq a => Pattern a (Graph a)
distFOPattern = conn (atomOf FO) (atomOf FOE) [roPort] [miPort]

pruneAPattern :: Eq a => Pattern a (Graph a)
pruneAPattern = conn (atomOf A) (atomOf T) [moPort] [miPort]

pruneFIPattern :: Eq a => Pattern a (Graph a)
pruneFIPattern = conn (atomOf FI) (atomOf T) [moPort] [miPort]

pruneLPattern :: Eq a => Pattern a (Graph a)
pruneLPattern = conn (atomOf L) (atomOf T) [loPort] [miPort]

pruneFObPattern :: Eq a => Pattern a (Graph a)
pruneFObPattern = conn (atomOf FO) (atomOf T) [loPort] [miPort]

pruneFOEbPattern :: Eq a => Pattern a (Graph a)
pruneFOEbPattern = conn (atomOf FOE) (atomOf T) [loPort] [miPort]

pruneFOcPattern :: Eq a => Pattern a (Graph a)
pruneFOcPattern = conn (atomOf FO) (atomOf T) [roPort] [miPort]

pruneFOEcPattern :: Eq a => Pattern a (Graph a)
pruneFOEcPattern = conn (atomOf FOE) (atomOf T) [roPort] [miPort]

