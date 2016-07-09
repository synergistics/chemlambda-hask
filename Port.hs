module Port where

data Port a
  = Li { portId :: a }
  | Ri { portId :: a }
  | Mi { portId :: a }
  | Lo { portId :: a }
  | Ro { portId :: a }
  | Mo { portId :: a }
  deriving ( Eq, Ord )

instance Show a => Show (Port a) where
  show (Li a) = "Li " ++ (show a)
  show (Ri a) = "Ri " ++ (show a)
  show (Mi a) = "Mi " ++ (show a)
  show (Lo a) = "Lo " ++ (show a)
  show (Ro a) = "Ro " ++ (show a)
  show (Mo a) = "Mo " ++ (show a)


isLi :: Port a -> Bool
isLi (Li _) = True
isLi _      = False

isRi :: Port a -> Bool
isRi (Ri _) = True
isRi _      = False

isMi :: Port a -> Bool
isMi (Mi _) = True
isMi _      = False

isLo :: Port a -> Bool
isLo (Lo _) = True
isLo _      = False

isRo :: Port a -> Bool
isRo (Ro _) = True
isRo _      = False

isMo :: Port a -> Bool
isMo (Mo _) = True
isMo _      = False

-- Return the complementary port to a given one
complement :: Port a -> Port a
complement (Li a) = (Lo a)
complement (Ri a) = (Ro a)
complement (Mi a) = (Mo a)
complement (Lo a) = (Li a) 
complement (Ro a) = (Ri a) 
complement (Mo a) = (Mi a) 


