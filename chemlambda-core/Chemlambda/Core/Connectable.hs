module Chemlambda.Core.Connectable 
  ( Connectable(..) )
  where

-- | Class for types whose elements can connect to one another
class Connectable a where
  connects :: a -> a -> Bool
