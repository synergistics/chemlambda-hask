{-# LANGUAGE ViewPatterns #-}

module Chemlambda.Node where 

import qualified Data.List as L
import Util.Graph

data Node
  = L     
  | A     
  | FI    
  | FO    
  | FOE   
  | Arrow 
  | T     
  | FRIN  
  | FROUT 
  deriving ( Show, Read, Eq )

lam :: Int -> Int -> Int -> Context Node Int
lam mi lo ro                         = Context [mi] L [lo, ro]

app :: Int -> Int -> Int -> Context Node Int
app li ri mo                         = Context [li, ri] A [mo]

fi :: Int -> Int -> Int -> Context Node Int
fi li ri mo                          = Context [li, ri] FI [mo]

fo :: Int -> Int -> Int -> Context Node Int
fo mi lo ro                          = Context [mi] FO [lo, ro]

foe :: Int -> Int -> Int -> Context Node Int
foe mi lo ro                         = Context [mi] FOE [lo, ro]

arrow :: Int -> Int -> Context Node Int
arrow mi mo                          = Context [mi] Arrow [mo]

ter :: Int -> Context Node Int
ter mi                               = Context [mi] T []

frin :: Int -> Context Node Int
frin mo                              = Context [] FRIN [mo]

frout :: Int -> Context Node Int
frout mi                             = Context [mi] FROUT []
