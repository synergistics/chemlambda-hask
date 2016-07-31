module ReactionSitesBench where

import Criterion.Main
import Chemlambda.Chemistry.Reaction
import Chemlambda.Chemistry.Rewrite.Deterministic
import Chemlambda.Chemistry.Rewrite.Random
import Chemlambda.Chemistry.Rewrite.Util
import Chemlambda.SampleData.Graphs

run = defaultMain 
  [ bench "deterministic" $ whnf (rewriteIter detRewrite 10) longIdentity
  , bench "random" $ whnf (rewriteIterIO randRewrite 10) longIdentity
  ]

-- run = rewriteIter detRewrite 10 longIdentity
-- run = rewriteIterIO randRewrite 10 longIdentity
 
