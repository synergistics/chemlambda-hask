{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Chemlambda.Pretty where

import Data.List

import Chemlambda.Node
import Util.Graph 


class Pretty a where
  pretty :: a -> String

instance (Show n, Show e) => Pretty (Context n e) where
  pretty c = show (node c) ++ "\t" ++ show (inEdges c ++ outEdges c)

instance (Show n, Show e) => Pretty (Graph n e) where
  pretty g = intercalate "\n" (map pretty g)

prettyP :: (Pretty a) => a -> IO ()
prettyP = putStrLn . pretty

