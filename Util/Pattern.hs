module Util.Pattern where

import Data.Monoid 
import Data.List

data Pattern a = Pattern (a -> Bool) 

instance Monoid (Pattern a) where
  mempty = Pattern (const True)
  mappend (Pattern p) (Pattern q) = Pattern ((&&) <$> p <*> q)


match :: Pattern a -> a -> Maybe a
match (Pattern p) a =
  if p a
    then Just a
    else Nothing 

