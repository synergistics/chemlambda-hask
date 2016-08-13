module Main where

import System.Environment
import Chemlambda.Core.Graph
import Chemlambda.Chemistry.Rewrite.Random
import Chemlambda.Chemistry.Rewrite.Deterministic
import Chemlambda.Chemistry.Rewrite.Util
import Chemlambda.Language.MolParser.Parser

main :: IO ()
main = do
  [fileName] <- getArgs
  molFile    <- readFile fileName
  let (Right a) = parseMol molFile
  let a' = length $ nodes $ rewriteIterFull detRewrite a
  -- a' <- rewriteIterIO randRewrite 200 a
  print a'
  
