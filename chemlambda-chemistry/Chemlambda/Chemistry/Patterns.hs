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

betaPattern :: Eq a => Pattern [Node a] (Graph [Node a])
betaPattern = conn (atomOf L) (atomOf A) [roPort] [liPort]

combPattern :: Eq a => Pattern [Node a] (Graph [Node a])
combPattern = conn anyNode (atomOf ARROW) [loPort,roPort,moPort] [miPort]

fanInPattern :: Eq a => Pattern [Node a] (Graph [Node a])
fanInPattern = conn (atomOf FI) (atomOf FOE) [moPort] [miPort]

distLPattern :: Eq a => Pattern [Node a] (Graph [Node a])
distLPattern = conn (atomOf L) (atomOf FO <|> atomOf FOE) [roPort] [miPort]

distAPattern :: Eq a => Pattern [Node a] (Graph [Node a])
distAPattern = conn (atomOf A) (atomOf FO <|> atomOf FOE) [moPort] [miPort]

distFIPattern :: Eq a => Pattern [Node a] (Graph [Node a])
distFIPattern = conn (atomOf FI) (atomOf FO) [moPort] [miPort]

distFOPattern :: Eq a => Pattern [Node a] (Graph [Node a])
distFOPattern = conn (atomOf FO) (atomOf FOE) [roPort] [miPort]

pruneAPattern :: Eq a => Pattern [Node a] (Graph [Node a])
pruneAPattern = conn (atomOf A) (atomOf T) [moPort] [miPort]

pruneFIPattern :: Eq a => Pattern [Node a] (Graph [Node a])
pruneFIPattern = conn (atomOf FI) (atomOf T) [moPort] [miPort]

pruneLPattern :: Eq a => Pattern [Node a] (Graph [Node a])
pruneLPattern = conn (atomOf L) (atomOf T) [loPort] [miPort]

pruneFObPattern :: Eq a => Pattern [Node a] (Graph [Node a])
pruneFObPattern = conn (atomOf FO) (atomOf T) [loPort] [miPort]

pruneFOEbPattern :: Eq a => Pattern [Node a] (Graph [Node a])
pruneFOEbPattern = conn (atomOf FOE) (atomOf T) [loPort] [miPort]

pruneFOcPattern :: Eq a => Pattern [Node a] (Graph [Node a])
pruneFOcPattern = conn (atomOf FO) (atomOf T) [roPort] [miPort]

pruneFOEcPattern :: Eq a => Pattern [Node a] (Graph [Node a])
pruneFOEcPattern = conn (atomOf FOE) (atomOf T) [roPort] [miPort]

