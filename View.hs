{-# LANGUAGE GADTs #-}

module View where

import Atom
import Graph


data View a where
  View :: Atom a => a -> Graph a -> View a


