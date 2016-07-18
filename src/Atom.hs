module Atom 
  ( Atom(..) )
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
