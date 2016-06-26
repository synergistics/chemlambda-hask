module Matching where

class Matching a where
  match :: a -> a -> Bool

