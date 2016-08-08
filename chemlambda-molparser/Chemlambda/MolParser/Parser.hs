{- LANGUAGE PartialTypeSignatures -}

module Chemlambda.MolParser.Parser where

import Data.Maybe ( fromJust )
import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Graph

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


atomP :: Parsec String (Int, Map.Map String Int) Atom
atomP =
  let 
    as = map try 
      [ (string "L")     >> return L
      , (string "FOE")   >> return FOE
      , (string "FO")    >> return FO
      , (string "A")     >> return A
      , (string "FI")    >> return FI
      , (string "ARROW") >> return ARROW
      , (string "FRIN")  >> return FRIN
      , (string "FROUT") >> return FROUT
      , (string "T")     >> return T
      ]
  in choice as 


-- nodeP :: Parsec String (Int, Map.Map String Int) (Node Int)
nodeP = do
  a <- between spaces (many1 space) atomP
  -- IAaMFG
  words <- count (valence a) word 
  
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
          L -> 
            let [a,b,c] = ports
            in lam a b c
          FO -> 
            let [a,b,c] = ports
            in fo a b c
          FOE -> 
            let [a,b,c] = ports
            in foe a b c
          A -> 
            let [a,b,c] = ports
            in app a b c
          FI -> 
            let [a,b,c] = ports
            in fi a b c
          ARROW -> 
            let [a,b] = ports
            in arrow a b
          FRIN -> 
            let [a] = ports
            in frin a
          FROUT -> 
            let [a] = ports
            in frout a
          T -> 
            let [a] = ports
            in t a

molP :: Parsec String (Int, Map.Map String Int) (Graph Int)
molP = mkGraph <$> nodeP `sepBy` spaces
  
parseMol :: String -> Either ParseError (Graph Int)
parseMol = runParser molP (0, Map.empty) ""
