{-# LANGUAGE FlexibleContexts #-}

module Util.Graph where

import qualified Data.List as L


data Context n e = Context
  { inEdges :: [e]
  , node :: n
  , outEdges :: [e] } 
  deriving ( Show, Read, Eq )


type Graph n e = [Context n e]


data View n e = (:&)
  { ctx :: Context n e
  , graph :: Graph n e } 
  deriving ( Show, Read, Eq )


toViews :: Graph n e -> [View n e]
toViews g = map (:& g) g

remove :: (Eq n, Eq e) => Context n e -> Graph n e -> Graph n e 
remove c g = filter (/= c) g 

removeMult :: (Eq n, Eq e) => [Context n e] -> Graph n e -> Graph n e 
removeMult cs g = filter (\c -> not (elem c cs)) g
