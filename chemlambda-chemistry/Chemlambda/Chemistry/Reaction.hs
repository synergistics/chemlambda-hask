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
        gen <- newStdGen
        let 
          pairs  = zip rsites (randoms gen :: [Bool]) 
          picked =                                
            foldr                                      
              (\(rsite, picked) rsitesAcc ->           
                if picked                              
                  then rsite:rsitesAcc                 
                  else rsitesAcc)                      
              []                                       
              pairs                                    
        return $ nubBy (\rsA rsB -> sitesOverlap rsA rsB) picked
  in
    randomChoiceSites

deterministicReactionSites :: Eq a => Graph [Node a] -> [[Enzyme a]] -> [ReactionSite a]
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

deterministicReactionSites graph lolEnzymes =
  let
    rsites = concatMap (concatMap (\enzyme -> reactionSites enzyme graph)) lolEnzymes
  in
    nubBy (\rsA rsB -> sitesOverlap rsA rsB) rsites 
-- reactionSitesMult :: Eq a => [Enzyme a] -> Graph [Node a] -> [ReactionSite a]
-- reactionSitesMult enzymes graph =
--   concatMap (\enzyme -> reactionSites enzyme graph) enzymes

-- Decent
-- runEnzyme
--   :: (Ord a, Enum a)
--   => Enzyme a
--   -> Graph [Node a]
--   -> Graph [Node a]
-- runEnzyme enzyme graph =
--   let
--     rsites = reactionSites enzyme graph
--   in foldl' (flip reactInGraph) graph rsites
