module Chemlambda.Language.Chemeral.Definition where

import Data.Maybe (fromJust)
import qualified Data.Map as M
import Chemlambda.Core.Node
import Chemlambda.Core.Port

type Namespace = String

data Var a 
  = Structural { varNamespace :: Namespace, val :: a } 
  | Parameter  { varNamespace :: Namespace, param :: String }
  deriving ( Eq, Show, Ord )

isStructural (Structural _ _) = True
isStructural _              = False

isParameter (Parameter _ _) = True
isParameter _              = False

-- applyMolDef (fromList 
--   [ ("a", Structural "K" 1)
--   , ("b", Parameter "K" "b") ]) 

-- With this implementation, all invoked moldefs such as "Id 1" are 
-- turned into nodes eagerly, i.e. when they are invoked.
-- Maybe defNodes could be made into thunks of themselves? Or is that even
-- necessary due to haskell's laziness. Note to self: find out!
data MolDef a = MolDef
  { molNamespace :: Namespace 
  , params       :: [Var a]
  , defNodes     :: [Node (Var a)] 
  , runDef       :: [Var a] -> [Node (Var a)] }

-- data MolInvoke a = MolInvoke { invokedDef :: MolDef a, args :: [Var a] }

-- runInvocation :: MolInvoke a -> [Node (Var a)]
-- runInvocation md = runDef (invokedDef md) (args md)

replaceVar :: Eq a => Var a -> Var a -> Node (Var a) -> Node (Var a)
replaceVar v1 v2 (Node a pvs) = Node a $ map (fmap (\v -> if v == v1 then v2 else v)) pvs 

lMolDef :: MolDef a
lMolDef =
  let
    namespace = "PRIM_L"
    params    = []
    defNodes  = []
    runDef    = \[a,b,c] -> [lam a b c]
  in
    MolDef namespace params defNodes runDef

mkMolDef :: (Ord a, Enum a) => Namespace -> [Var a] -> [Node (Var a)] -> MolDef a
mkMolDef nsp params nodes =
  let
    runDef' = \args ->  
      let
        paramMap = M.fromList $ zip params args
      in
        map 
          (\(Node a ps) -> Node a $ 
            map (fmap (\v -> M.findWithDefault v v paramMap)) ps)
          nodes  
  in
    MolDef nsp params nodes runDef'

idMolDef :: (Ord a, Enum a, Num a) => MolDef a
idMolDef = mkMolDef "ID" 
                    [Parameter "ID" "c"] 
                    (concat 
                      [ runDef lMolDef 
                        [ (Structural "ID" 1)
                        , (Structural "ID" 1)
                        , (params idMolDef !! 0)] ]) 

  -- let
  --   namespace = "ID"
  --   params    = [Parameter "ID" "c"]
  --   defNodes  = concat [runDef lMolDef [(Structural "ID" 1), (Structural "ID" 1), (params !! 0)]]
  --   runDef'   = \[c] -> map (replaceVar (params !! 0) c) defNodes 
  -- in
  --   MolDef namespace params defNodes runDef'

-- unusedStructuralVars :: (Ord a, Enum a) => MolDef a -> [Var a]
-- unusedStructuralVars md =
--   let
--     start = succ 
--           $ maximum 
--           $ map val 
--           $ filter isStructural 
--           $ concatMap args
--           $ molCalls md
--     newVars = iterate succ start 
--   in map (Structural (molNamespace md)) newVars

-- appendInvokes :: (Ord a, Enum a) => MolDef a -> [MolInvoke a] -> MolDef a 
-- appendInvokes md mis =
--   let
--     mdName = molNamespace md
--     unused = unusedStructuralVars md
--     structurals = filter isStructural $ concatMap args mis

--     m = M.fromList $ zip structurals unused

--     go (MolInvoke md' pvs) = MolInvoke md' $ map (\v -> if isStructural v && varNamespace v /= molNamespace md then fromJust $ M.lookup v m else v) pvs
--   in md { molCalls = molCalls md ++ map go mis }
--   -- in structurals

-- -- expandDef :: MolDef a -> [Var a] -> MolDef a -> [Node (Var a)]
-- -- expandDef mdWithin vars mdEnclosing = 

-- -- make fully parameterized MolDefs for the standard nodes.
-- -- namespace = "PRIMITIVE-APP
-- -- idMol :: (Ord a, Enum a, Num a) => MolDef a
-- -- idMol = 
-- --   let
-- --     namespace = "ID"
-- --     defNodes  = [ lam (Structural "ID" 1) (Structural "ID" 1) (Parameter "ID" "a") ]

-- --     runDef vs = map (replaceVar (Parameter "ID" "a") (vs !! 0)) defNodes  
-- --       where
-- --         
-- --   in
-- --     MolDef namespace defNodes runDef




-- -- kMol :: MolDef a
-- -- kMol = MolDef $ \[a] -> [runDef (getEnv kMol) idMol 2, l 2 1 a, t 1]
