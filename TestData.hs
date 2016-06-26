module TestData where 

import Chemlambda.Node
import Chemlambda.Moves
import Chemlambda.Pretty
import Util.Graph
import qualified Chemlambda.Graph as CG


testGraph = 
  [ frin 1 
  , lam 1 1 3
  , app 3 4 5 
  , ter 5
  , frin 4 ] 

testView = testGraph !! 1 :& testGraph
