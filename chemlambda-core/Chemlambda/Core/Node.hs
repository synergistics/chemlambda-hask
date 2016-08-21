module Chemlambda.Core.Node
  where

import qualified Data.List as List
import Chemlambda.Core.Port


data NodeRef a = NR { nRef :: a, nPT :: PortType }
  deriving ( Eq )

data Node a b = Node { atom :: a, refs :: [ NodeRef b ] }
  deriving ( Eq, Show )

instance Show a => Show (NodeRef a) where
  show (NR i pt) = "(NR " ++ show i ++ " " ++ show pt ++ ")"


refAtPort :: Node a b -> PortType -> Maybe (NodeRef b)
refAtPort node pt
  = List.find (\(NR _ pt') -> pt' == pt)
  $ refs node
