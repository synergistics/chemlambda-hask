
module Main where

import TestData
import Chemlambda.Pretty
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Core.Reaction
import Chemlambda.Standard.Rewrite
-- import Test.Hspec 

rewriteCycle times graph = iterate rewrite graph !! times
main :: IO ()
main = print $ rewriteCycle 100 y 
