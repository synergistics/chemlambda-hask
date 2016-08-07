{- LANGUAGE PartialTypeSignatures -}

module Chemlambda.MolParser.Parser where

import Data.Maybe
import qualified Chemlambda.Core.Port as Port
import qualified Chemlambda.Core.Atom as Atom
import qualified Chemlambda.Core.Graph as Graph

import Chemlambda.Core.Node
import qualified Data.Map as Map
-- import Control.Monad.Writer
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
-- import System.Random
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Error


atomP :: Parsec String (Int, Map.Map String Int) Atom.Atom
atomP =
  let 
    as = map try 
      [ (string "L")     >> return Atom.L
      , (string "FOE")   >> return Atom.FOE
      , (string "FO")    >> return Atom.FO
      , (string "A")     >> return Atom.A
      , (string "FI")    >> return Atom.FI
      , (string "ARROW") >> return Atom.ARROW
      , (string "FRIN")  >> return Atom.FRIN
      , (string "FROUT") >> return Atom.FROUT
      , (string "T")     >> return Atom.T
      ]
  in choice as 


-- nodeP :: Parsec String (Int, Map.Map String Int) (Node Int)
nodeP = do
  a <- between spaces (many1 space) atomP
  -- IAaMFG
  words <- count (Atom.valence a) word 
  
  forM_ words $ \w -> do
    modifyState (\(i, m) -> if Map.notMember w m 
                              then (i+1, Map.insert w i m)
                              else (i, m))

  (i, m) <- getState
  return (toNode a words m) 

  where
    word = do
      w <- many1 (alphaNum <|> char '_')
      spaces
      return w
    toPorts words m = map (\w -> fromJust $ Map.lookup w m) words 
    toNode atom portNames m = 
      let ports = toPorts portNames m in
        case atom of
          Atom.L -> 
            let [a,b,c] = ports
            in lam a b c
          Atom.FO -> 
            let [a,b,c] = ports
            in fo a b c
          Atom.FOE -> 
            let [a,b,c] = ports
            in foe a b c
          Atom.A -> 
            let [a,b,c] = ports
            in app a b c
          Atom.FI -> 
            let [a,b,c] = ports
            in fi a b c
          Atom.ARROW -> 
            let [a,b] = ports
            in arrow a b
          Atom.FRIN -> 
            let [a] = ports
            in frin a
          Atom.FROUT -> 
            let [a] = ports
            in frout a
          Atom.T -> 
            let [a] = ports
            in t a

molP :: Parsec String (Int, Map.Map String Int) (Graph.Graph [Node Int])
molP = Graph.Graph <$> nodeP `sepBy` spaces
  
parseMol :: String -> Either ParseError (Graph.Graph [Node Int])
parseMol = runParser molP (0, Map.empty) ""
