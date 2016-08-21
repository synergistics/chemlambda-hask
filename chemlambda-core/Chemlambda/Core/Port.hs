module Chemlambda.Core.Port
  where


data PortType = LO | LI | RO | RI | MO | MI
  deriving ( Eq, Show )

data Direction = I | O
  deriving ( Eq, Ord, Show )

isOut :: PortType -> Bool
isOut = flip elem [LO, RO, MO]

isIn :: PortType -> Bool
isIn = flip elem [LI, RI, MI]

direction :: PortType -> Direction
direction p
  | isOut p = O
  | isIn  p = I

