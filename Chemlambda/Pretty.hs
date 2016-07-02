{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Chemlambda.Pretty where

import Data.List

import Chemlambda.Node
import Util.Graph 

class Pretty a where
  pretty :: a -> String

instance (Show n, Show e) => Pretty (Context n e) where
  pretty c = (pad (show $ node c) 5) ++ show (inEdges c ++ outEdges c)
    where
      pad :: String -> Int -> String
      pad s n =
        let ns = n - length s
        in s ++ replicate ns ' '

instance (Show n, Show e) => Pretty (Graph n e) where
  pretty g = intercalate "\n" (map pretty g)

instance (Show n, Show e) => Pretty (View n e) where
  pretty v = "(:&) " ++ (pretty $ ctx v) ++ (concat $ map ("\n\t"++) (lines $ pretty $ graph v)) 

prettyP :: (Pretty a) => a -> IO ()
prettyP = putStrLn . pretty

