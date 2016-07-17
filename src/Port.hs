module Port 
  ( Port(..) 
  , isLi, isRi, isMi, isLo, isRo, isMo
  )
  where

import Connectable


-- A port for a Chemlambda atom 
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

instance Functor Port where
  fmap f (Li a) = Li $ f a
  fmap f (Ri a) = Ri $ f a
  fmap f (Mi a) = Mi $ f a
  fmap f (Lo a) = Lo $ f a
  fmap f (Ro a) = Ro $ f a
  fmap f (Mo a) = Mo $ f a

instance Eq a => Connectable (Port a) where
  connects p q = portId p == portId q && p /= q

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

complement :: Port a -> Port a
complement (Li a) = Lo a
complement (Ri a) = Ro a
complement (Mi a) = Mo a
complement (Lo a) = Li a 
complement (Ro a) = Ri a 
complement (Mo a) = Mi a 
