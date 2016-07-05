module Pattern where

import Graph
import View


match :: (Eq proto) => Graph proto -> Graph concrete -> Maybe (View concrete)
match 
