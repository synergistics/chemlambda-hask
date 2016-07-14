module Reduction where

import Pattern
import Moves
import Graph
import GraphMod

type GraphModPredicate a = GraphMod a -> Bool
reduceGeneral
  :: (Eq a, Enum a)
  => [[PatternMove a]]
  -> GraphModPredicate a 
  -> Graph a
  -> Graph a
reduceGeneral patternMoveLists pred graph =
  let
    proposedMods = allGraphMods patternMoveLists graph
    acceptedMods = filter pred proposedMods
  in
    foldr (\mod graph -> applyGraphMod mod graph) graph acceptedMods  

standardPatternMoveList :: Ord a => [[PatternMove a]]
standardPatternMoveList = 
  [ [ (distFOPattern , distFOMove) ]

  , [ (distAPattern  , distAMove)
    , (distLPattern  , distLMove)
    , (distFIPattern , distFIMove) ]

  , [ (betaPattern   , betaMove)
    , (fanInPattern  , fanInMove)  ] ]
