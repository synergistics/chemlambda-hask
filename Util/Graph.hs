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

-- -- [e] requires flex contexts because its a concrete type
-- instance (Matching n, Matching [e]) => Matching (Context n e) where
--   match c1 c2 =  
--     match (ies c1) (ies c2) && 
--     match (node c1) (node c2) && 
--     match (oes c1) (oes c2)


-- -- nodeContext :: (Eq n, Eq e) => n -> Graph n e -> Maybe (Context n e) 
-- -- nodeContext n graph = L.find (\c -> n == node c) graph 

-- -- inContexts :: (Eq n, Eq e) => Context n e -> Graph n e -> Graph n e 
-- -- inContexts (Context ie  ) graph = filter (\(Context   oe) -> any (`elem` oe) ie) graph
     
-- -- outContexts :: (Eq n, Eq e) => Context n e -> Graph n e -> Graph n e 
-- -- outContexts (Context   oe) graph = filter (\(Context ie  ) -> any (`elem` ie) oe) graph


