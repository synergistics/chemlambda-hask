module Chemlambda.Chemistry.Reaction
  ( Enzyme(..)
  , ReactionSite(..)
  , sitesOverlap
  , runReaction
  , reactionSites
  , reactInGraph
  , randomReactionSites
  , deterministicReactionSites
  )
  where

-- Test performance differences with reacting automatically (lazily so not really) versus
-- going through the intermediate step of ReactionSite
import Data.List
import Data.List.Extra
import System.Random
import Chemlambda.Core.Port
import Chemlambda.Core.Node
import Chemlambda.Core.Graph
import Chemlambda.Core.Pattern


data Enzyme a = Enzyme
  { pattern :: Pattern [Node a] (Graph [Node a]) 
  , move    :: Graph [Node a] -> Graph [Node (NewId a)] }

data ReactionSite a = ReactionSite 
  { site     :: Graph [Node a]
  , reaction :: Graph [Node a] -> Graph [Node (NewId a)] } 

runReaction :: ReactionSite a -> Graph [Node (NewId a)]
runReaction rsite = reaction rsite $ site rsite

reactionSites :: Enzyme a -> Graph [Node a] -> [ReactionSite a] 
reactionSites enzyme graph =
  let matches = match (pattern enzyme) graph
  in map (\graph -> ReactionSite graph (move enzyme)) matches

sitesOverlap :: Eq a => ReactionSite a -> ReactionSite a -> Bool
sitesOverlap rsiteA rsiteB = 
  let
    siteA        = site rsiteA
    siteB        = site rsiteB
    Graph shared = intersect <$> siteA <*> siteB
  in
    not $ null shared

reactInGraph 
  :: (Enum a, Ord a) 
  => ReactionSite a 
  -> Graph [Node a] 
  -> Graph [Node a]
reactInGraph rsite graph =
  let
    withAdded   = graph `plusNew` runReaction rsite
    withRemoved = withAdded `minus` site rsite
  in withRemoved

randomReactionSites :: Eq a => Graph [Node a] -> [Enzyme a] -> IO [ReactionSite a]
randomReactionSites graph enzymes =
  let
    rsites            = concatMap (\enzyme -> reactionSites enzyme graph) enzymes
    randomChoiceSites = 
      do
        boolGen <- newStdGen
        numGen  <- newStdGen
        let 
          shuffleSites = sortBy $ \(_,_,a) (_,_,b) -> compare a b

          pairs  = shuffleSites $ zip3 rsites (randoms boolGen :: [Bool]) (randoms numGen :: [Int])
          picked =                                
            foldr                                      
              (\(rsite, picked, _) rsitesAcc ->           
                if picked                              
                  then rsite:rsitesAcc                 
                  else rsitesAcc)                      
              []                                       
              pairs                                    
        return $ nubBy (\rsA rsB -> sitesOverlap rsA rsB) picked
        -- return $ nubOrdBy (\rsA rsB -> if sitesOverlap rsA rsB then EQ else LT) picked
  in
    randomChoiceSites



deterministicReactionSites :: Eq a => Graph [Node a] -> [Enzyme a] -> [ReactionSite a]
deterministicReactionSites graph enzymes =
  let
    rsites = concatMap (\enzyme -> reactionSites enzyme graph) enzymes
    picked = drop (length rsites `div` 2) rsites
  in
    nubOrdBy (\rsA rsB -> if sitesOverlap rsA rsB then EQ else LT) picked
    -- nubBy (\rsA rsB -> sitesOverlap rsA rsB) picked 

-- deterministicReactionSites graph lolEnzymes = snd $ foldl'
-- -- (list of lists of enzymes)
--   (\(graph, rsites) enzymes ->
--     let
--       rsites' = concatMap (\enzyme -> reactionSites enzyme graph) enzymes
--       sites   = map site rsites'
--       graph'  = foldl' minus graph sites
--     in (graph', rsites' ++ rsites))
--   (graph, [])
--   lolEnzymes

