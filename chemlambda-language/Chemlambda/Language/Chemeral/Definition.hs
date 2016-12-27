module Chemlambda.Language.Chemeral.Definition where

import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Monad
import Chemlambda.Core.Node
import Chemlambda.Core.Port


type Namespace = String

data Var a = Structural { varNamespace :: Namespace, val :: a } 
  | Parameter  { varNamespace :: Namespace, param :: String }
  deriving ( Eq, Show, Ord )

isStructural (Structural _ _) = True
isStructural _                = False

isParameter (Parameter _ _) = True
isParameter _               = False

-- With this implementation, all invoked moldefs such as "Id 1" are 
-- turned into nodes eagerly, i.e. when they are invoked.
-- Maybe defNodes could be made into thunks of themselves? Or is that even
-- necessary due to haskell's laziness. Note to self: find out!
data MolDef a = MolDef
  { molNamespace :: Namespace -- not the same as the name of the def
  , params       :: [Var a]
  , molCalls     :: [MolInvoke a] }
  deriving ( Show )

data MolInvoke a 
  = Thunked { invokedDef :: MolDef a, args :: [Var a] }
  | Invoked { node :: Node (Var a) }
  deriving ( Show )
  
-- instance Functor (MolDef ir) where
--   fmap f md = md { ret = f $ ret md }

-- instance Applicative (MolDef ir) where
--   pure a     = MolDef "" [] [] (const []) a
--   mdf <*> md = fmap (ret mdf) md

-- instance Monad (MolDef ir) where -- The monad is realized by the fact that a MolDef is a set of molCalls in a context
--   return = pure
--   md >>= f = f (ret md) 

-- getField :: (MolDef ir r -> s) -> MolDef ir r -> MolDef ir s
-- getField field md = const (field md) <$> md

repVars :: Ord a => M.Map (Var a) (Var a) -> [MolInvoke a] -> [MolInvoke a] 
repVars varMap mis = map go mis 
  where
    go (Thunked d as) = Thunked d $ map (\a -> M.findWithDefault a a varMap) as 
    go (Invoked n)    = Invoked $ n { ports = liftM (fmap (\p -> M.findWithDefault p p varMap)) (ports n) }

runDef :: Ord a => MolDef a -> [Var a] -> [MolInvoke a]
runDef md args = 
  let
    paramMap = M.fromList $ zip (params md) args
  in
    repVars paramMap (molCalls md)

expandInvoke :: Ord a => MolInvoke a -> [MolInvoke a]
expandInvoke (Thunked d as) = concatMap expandInvoke (runDef d as)
expandInvoke (Invoked n)    = [Invoked n]

lamMol = 
  MolDef "prim_l" 
         [ Parameter "prim_l" "a" 
         , Parameter "prim_l" "b" 
         , Parameter "prim_l" "c" ]
         [ Invoked $ lam (params lamMol !! 0) (params lamMol !! 1) (params lamMol !! 2) ]        

idMol = 
  MolDef "ID"
         [ Parameter "ID" "c" ]
         $ concat [ runDef lamMol [Structural "ID" 1, Structural "ID" 1, params idMol !! 0] ]

kMol =
  MolDef "K"
         [ Parameter "K" "c" ]
         $ concat [ runDef idMol  [Structural "K" 1]
                  , runDef lamMol [Structural "K" 1, Structural "K" 2, Parameter "K" "c"] ]
         
-- structurals :: MolInvoke a -> [Var a]
-- structurals mi = 
  -- let
    -- allVars = case mi of
          --       (Thunked d as) -> as
          --       (Invoked n)    -> map portId $ ports n
  -- in filter isStructural allVars


-- addInvocations :: (Ord a, Enum a) => [MolInvoke a] -> MolDef a -> MolDef a
-- addInvocations mis md =
  -- let
    -- unused     = unusedStructuralVars md
    -- invokeVars = nub $ filter (\s -> varNamespace s /= molNamespace md) $ concatMap structurals mis -- replace nub with nubOrd if possible
    -- structuralMap = M.fromList $ zip invokeVars unused
    -- additions  = map go mis
      -- where
        -- go (Thunked d as) = Thunked d (replaceVar structuralMap as)
        -- go (Invoked n)    = Invoked $ n { ports = map (fmap (head . (replaceVar structuralMap) . return)) (ports n) }
  -- in
    -- md {molCalls = molCalls md ++ additions}

-- expandInvocation 

-- unusedStructuralVars :: (Ord a, Enum a) => MolDef a -> [Var a]
-- unusedStructuralVars md =
  -- let
    -- start = succ 
          -- $ maximum 
          -- $ map val 
          -- $ concatMap structurals
          -- $ molCalls md
    -- newVars = iterate succ start 
  -- in map (Structural (molNamespace md)) newVars


































































































-- -- -- replaceVar :: Eq a => Var a -> Var a -> Node (Var a) -> Node (Var a)
-- -- -- replaceVar v1 v2 (Node a pvs) = Node a $ map (fmap (\v -> if v == v1 then v2 else v)) pvs 

-- -- mkMolDef :: (Ord a, Enum a) => Namespace -> [Var a] -> [Node (Var a)] -> MolDef a
-- -- mkMolDef nsp params nodes =
-- --   let
-- --     runDef' = \args ->  
-- --       let
-- --         paramMap = M.fromList $ zip params args
-- --       in
-- --         map 
-- --           (\(Node a ps) -> Node a $ 
-- --             map (fmap (\v -> M.findWithDefault v v paramMap)) ps)
-- --           nodes  
-- --   in
-- --     MolDef nsp params nodes runDef'

-- -- -- lamMol :: (Ord a, Enum a) => MolDef a
-- -- -- lamMol = 
-- -- --   let
-- -- --     params@[a,b,c] = [Parameter "ID" "a", Parameter "ID" "b", Parameter "ID" "c"]
-- -- --     nodes          = [lam a b c]
-- -- --   in
-- -- --     mkMolDef "PRIM_L" params nodes 

-- -- -- idMolDef :: (Ord a, Enum a, Num a) => MolDef a
-- -- -- idMolDef = mkMolDef "ID" 
-- -- --                     [Parameter "ID" "c"] 
-- -- --                     (concat 
-- -- --                       [ runDef lamMol 
-- -- --                         [ (Structural "ID" 1)
-- -- --                         , (Structural "ID" 1)
-- -- --                         , (params idMolDef !! 0)] ]) 

-- --   -- let
-- --   --   namespace = "ID"
-- --   --   params    = [Parameter "ID" "c"]
-- --   --   defNodes  = concat [runDef lMolDef [(Structural "ID" 1), (Structural "ID" 1), (params !! 0)]]
-- --   --   runDef'   = \[c] -> map (replaceVar (params !! 0) c) defNodes 
-- --   -- in
-- --   --   MolDef namespace params defNodes runDef'


-- -- -- appendInvokes :: (Ord a, Enum a) => MolDef a -> [MolInvoke a] -> MolDef a 
-- -- -- appendInvokes md mis =
-- -- --   let
-- -- --     mdName = molNamespace md
-- -- --     unused = unusedStructuralVars md
-- -- --     structurals = filter isStructural $ concatMap args mis

-- -- --     m = M.fromList $ zip structurals unused

-- -- --     go (MolInvoke md' pvs) = MolInvoke md' $ map (\v -> if isStructural v && varNamespace v /= molNamespace md then fromJust $ M.lookup v m else v) pvs
-- -- --   in md { molCalls = molCalls md ++ map go mis }
-- -- --   -- in structurals

-- -- -- -- expandDef :: MolDef a -> [Var a] -> MolDef a -> [Node (Var a)]
-- -- -- -- expandDef mdWithin vars mdEnclosing = 

-- -- -- -- make fully parameterized MolDefs for the standard nodes.
-- -- -- -- namespace = "PRIMITIVE-APP
-- -- -- -- idMol :: (Ord a, Enum a, Num a) => MolDef a
-- -- -- -- idMol = 
-- -- -- --   let
-- -- -- --     namespace = "ID"
-- -- -- --     defNodes  = [ lam (Structural "ID" 1) (Structural "ID" 1) (Parameter "ID" "a") ]

-- -- -- --     runDef vs = map (replaceVar (Parameter "ID" "a") (vs !! 0)) defNodes  
-- -- -- --       where
-- -- -- --         
-- -- -- --   in
-- -- -- --     MolDef namespace defNodes runDef




-- -- -- -- kMol :: MolDef a
-- -- -- -- kMol = MolDef $ \[a] -> [runDef (getEnv kMol) idMol 2, l 2 1 a, t 1]
