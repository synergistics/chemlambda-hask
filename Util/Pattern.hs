module Util.Pattern where

import Data.List

data Pattern a = Pattern (a -> Bool) 

(<&&>) :: Pattern a -> Pattern a -> Pattern a
(Pattern p) <&&> (Pattern q) = Pattern ((&&) <$> p <*> q)

(<||>) :: Pattern a -> Pattern a -> Pattern a
(Pattern p) <||> (Pattern q) = Pattern ((||) <$> p <*> q)

match :: Pattern a -> a -> Maybe a
match (Pattern p) a =
  if p a
    then Just a
    else Nothing 

