module Chemlambda.Core.AdjList
  where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.IntMap as IntMap
import Data.Map (Map) 
import Data.Maybe (Maybe) 
import Data.IntMap (IntMap, (!))

import Chemlambda.Core.Atom
import Chemlambda.Core.Port


type PortMap = Map PortType Int 

data AdjList a b = AdjList
  { entries :: IntMap (AdjEntry a b) }
  deriving (Ord, Eq, Show)

data AdjEntry a b = AdjEntry
  { node    :: a
  , portMap :: b }
  deriving (Ord, Eq, Show)


getEntry :: AdjList a b -> Int -> AdjEntry a b
getEntry = (!) . entries

addFrees :: [(Atom, [(PortType, Int)])] -> [(Atom, [(PortType, Int)])]
addFrees entries =
  let
    allPorts = concatMap snd entries

    withoutPair =
      filter
        (\(p,i)
          -> List.notElem i
          $ map snd
          $ allPorts List.\\ [(p,i)])
        allPorts

    frees =
      map
        (\(p,i) -> case direction p of
          O -> (FROUT, [(MI,i)])
          I -> (FRIN,  [(MO,i)]))
        withoutPair

  in entries ++ frees

-- subGraph :: [Int] -> Graph a -> Graph a
-- subGraph elems graph =
--   let
--     selectedNodes
--       = filter (\(i,_) -> elem i elems)
--       $ IntMap.assocs
--       $ unGraph graph
--   in Graph $ IntMap.fromList selectedNodes

-- nodeAtPort :: Node Atom Int -> PortType -> Graph Atom -> Maybe (Node Atom Int)
-- nodeAtPort node pt graph
--    =  getNode graph
--   <$> nRef
--   <$> refAtPort node pt


toAdjList :: [(Atom, [(PortType, Int)])] -> AdjList Atom PortMap
toAdjList portIdEntries =
  let
    portIdEntries' = addFrees portIdEntries
    indexedAtoms   = zip [0..] $ map fst portIdEntries' -- give an index to each atom

    portsWithAtomIndex :: [(Int, (PortType, Int))] -- give each port the index associated with its atom
    portsWithAtomIndex =
      let
        ports = map snd portIdEntries' 
        givePortsIndex i ps = map ((,) i) ps
      in concat $ zipWith givePortsIndex [0..] ports

    -- Make the port numbers of an atom refer to other atoms rather than port variable names
    toAtomRefPorts :: (Atom, [(PortType, Int)]) -> (Atom, [(PortType, Int)])
    toAtomRefPorts (a,ps) =
      let
        ps' = foldr (\(j,(p,i)) ps'' ->
          let
            withoutCurrentPort = ps List.\\ [(p,i)] -- Ensures that port variable must occur twice to be added 
            indices = map snd withoutCurrentPort 
          in
            if elem i indices
            then let
              matchingPort
                =  Maybe.fromJust 
                $  fmap (const j) -- \(p,_) -> (p,j)
               <$> List.find (\(_,i') -> i' == i) withoutCurrentPort
              in matchingPort : ps''
            else ps'')
          []
          portsWithAtomIndex
      in
        (a,ps')
  
    adjList
      = AdjList
      $ IntMap.fromList
      $ zip [0..] 
      $ map ((\(a,ps) -> AdjEntry a (Map.fromList ps)) . toAtomRefPorts)
      $ portIdEntries'

  in adjList 


lam a b c = (L,  [ (MI,a), (LO,b), (RO,c) ])
fo  a b c = (FO, [ (MI,a), (LO,b), (RO,c) ])
app a b c = (A,  [ (LI,a), (RI,b), (MO,c) ])

test :: [(Atom, [(PortType, Int)])]
test
  = concat
  $ take 1
  $ iterate (map (\(a, [(x,xx), (y,yy), (z,zz)]) -> (a, [(x,xx+200),(y,yy+200),(z,zz+200)]))) 
  [ app 2 3 4 
  , lam 1 1 2 
  , fo  8 9 10 ]
