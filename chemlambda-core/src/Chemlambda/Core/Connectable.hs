module Chemlambda.Core.Connectable 
  ( Connectable(..) )
  where

class Connectable a where
  connects :: a -> a -> Bool
