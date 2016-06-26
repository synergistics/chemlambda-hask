module TestData where 

import Chemlambda.Node
import Chemlambda.Moves
import Chemlambda.Pretty
import Util.Graph
import qualified Chemlambda.Graph as CG


testGraph = 
  [ frin 1 
  , lam 1 2 3
  , arrow 2 5
  , arrow 5 4
  , frout 4 ] 

testView = testGraph !! 1 :& testGraph
