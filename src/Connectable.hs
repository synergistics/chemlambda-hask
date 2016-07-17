module Connectable 
  ( Connectable(..) )
  where

class Connectable a where
  connects :: a -> a -> Bool
