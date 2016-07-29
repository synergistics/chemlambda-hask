module Main where

import Chemlambda.Core.Pattern
import Chemlambda.Core.Reaction
import Chemlambda.Standard.Enzymes
import Control.Parallel
import Control.Parallel.Strategies
import TestData
import Criterion.Main
import Criterion


a = map (flip reactionSites $ longIdentity) 
        (concat standardEnzymes)

b = parMap rpar (flip reactionSites $ longIdentity)
                (concat standardEnzymes)

main = defaultMain 
  [ bench "what the heck" $ whnf (const a) 0
  , bench "what the heck parallel" $ whnf (const b) 0]
