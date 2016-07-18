module Reduction where

import Pattern
import Moves
import Graph
import GraphModification

reduceGeneral
  :: (Eq a, Enum a)
  => [[PatternMove a]]
  -> (GraphModification a -> Bool)
  -> Graph a
  -> Graph a
reduceGeneral patternMoveLists pred graph =
  let
    proposedMods = allGraphModifications patternMoveLists graph
    acceptedMods = filter pred proposedMods
  in
    foldr applyGraphModification graph acceptedMods  


standardPatternMoveList :: Ord a => [[PatternMove a]]
standardPatternMoveList = 
  [ [ (distFOPattern , distFOMove) ]

  , [ (distAPattern  , distAMove)
    , (distLPattern  , distLMove)
    , (distFIPattern , distFIMove) ]

  , [ (betaPattern   , betaMove)
    , (fanInPattern  , fanInMove)  ] 
    
  , [ (prunePatterns , pruneMove)  ] ]
