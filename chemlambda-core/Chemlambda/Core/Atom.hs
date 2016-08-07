module Chemlambda.Core.Atom 
  ( Atom(..)
  , valence 
  )
  where

-- A Chemlambda atom
data Atom
  = L
  | FO
  | FOE
  | A
  | FI
  | ARROW
  | FRIN
  | FROUT
  | T
  deriving ( Show, Eq, Ord )

valence :: Num a => Atom -> a
valence a | elem a [FRIN,FROUT,T]  = 1
valence a | elem a [ARROW]         = 2
valence a | elem a [L,FO,FOE,A,FI] = 3

