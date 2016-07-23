
module Main where

import Test.Hspec 
import TestData
import Chemlambda.Pretty
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern
import Chemlambda.Core.Reaction
import Chemlambda.Standard.Rewrite

main :: IO ()
main = do
  n <- readLn :: IO Int
  pp $ rewriteCycle n meh
  main
