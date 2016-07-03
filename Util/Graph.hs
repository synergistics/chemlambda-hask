{-# LANGUAGE FlexibleContexts #-}

module Util.Graph where


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

removeCtx :: (Eq n, Eq e) => Context n e -> Graph n e -> Graph n e 
removeCtx c g = filter (/= c) g 

removeCtxs :: (Eq n, Eq e) => [Context n e] -> Graph n e -> Graph n e 
removeCtxs cs g = filter (\c -> not (elem c cs)) g

-- Maybe rename this
edges :: (Ord e) => Graph n e -> [e]
edges g = foldr (\c acc -> (inEdges c) ++ (outEdges c) ++ acc) [] g


