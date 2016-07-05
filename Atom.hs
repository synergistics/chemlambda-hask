module Atom where

import Port


data Atom a 
  = L     (Port a) (Port a) (Port a)
  | FO    (Port a) (Port a) (Port a)
  | FOE   (Port a) (Port a) (Port a)
  | A     (Port a) (Port a) (Port a)
  | FI    (Port a) (Port a) (Port a)
  | ARROW (Port a) (Port a)
  | FRIN  (Port a)
  | FROUT (Port a)
  | T     (Port a)
  deriving ( Show, Eq )


-- Smart constructors
lam :: a -> a -> a -> Atom a
lam a b c = L (In a) (Out b) (Out c)

fo :: a -> a -> a -> Atom a
fo a b c = FO (In a) (Out b) (Out c)

foe :: a -> a -> a -> Atom a
foe a b c = FOE (In a) (Out b) (Out c)

app :: a -> a -> a -> Atom a
app a b c = A (In a) (In b) (Out c)

fi :: a -> a -> a -> Atom a
fi a b c = FI (In a) (In b) (Out c)

arrow :: a -> a -> Atom a
arrow a b = ARROW (In a) (Out b)

frin :: a -> Atom a
frin a = FRIN (Out a)

frout :: a -> Atom a
frout a = FROUT (In a)

t :: a -> Atom a
t a = T (In a)


-- Port selectors
liPort :: Atom a -> Maybe a
liPort (A  (In a) _ _) = Just a
liPort (FI (In a) _ _) = Just a
liPort _ = Nothing

riPort :: Atom a -> Maybe a
riPort (A  _ (In b) _) = Just b
riPort (FI _ (In b) _) = Just b
riPort _ = Nothing

miPort :: Atom a -> Maybe a
miPort (L     (In a) _ _) = Just a 
miPort (FO    (In a) _ _) = Just a
miPort (FOE   (In a) _ _) = Just a
miPort (ARROW (In a) _)   = Just a
miPort (FROUT (In a))     = Just a
miPort (T     (In a))     = Just a
miPort _ = Nothing

loPort :: Atom a -> Maybe a
loPort (L   _ (Out b) _) = Just b
loPort (FO  _ (Out b) _) = Just b
loPort (FOE _ (Out b) _) = Just b
loPort _ = Nothing

roPort :: Atom a -> Maybe a
roPort (L   _ _ (Out c)) = Just c
roPort (FO  _ _ (Out c)) = Just c
roPort (FOE _ _ (Out c)) = Just c
roPort _ = Nothing

moPort :: Atom a -> Maybe a
moPort (A     _ _ (Out c)) = Just c
moPort (FI    _ _ (Out c)) = Just c
moPort (ARROW _ (Out b))   = Just b
moPort (FRIN  (Out a))     = Just a
moPort _ = Nothing


-- Constructor checks
isL :: Atom a -> Bool
isL (L _ _ _) = True
isL _ = False

isFO :: Atom a -> Bool
isFO (FO _ _ _) = True
isFO _ = False

isFOE :: Atom a -> Bool
isFOE (FOE _ _ _) = True
isFOE _ = False

isA :: Atom a -> Bool
isA (A _ _ _) = True
isA _ = False

isFI :: Atom a -> Bool
isFI (FI _ _ _) = True
isFI _ = False

isArrow :: Atom a -> Bool
isArrow (ARROW _ _) = True
isArrow _ = True

isFRIN :: Atom a -> Bool
isFRIN (FRIN _) = True
isFRIN _ = False

isFROUT :: Atom a -> Bool
isFROUT (FROUT _) = True
isFROUT _ = False

isT :: Atom a -> Bool
isT (T _) = True
isT _ = False


-- In and Out getters
inPorts :: Atom a -> [a]
inPorts (L     (In a) _ _)      = [a]
inPorts (FO    (In a) _ _)      = [a]
inPorts (FOE   (In a) _ _)      = [a]
inPorts (A     (In a) (In b) _) = [a,b]
inPorts (FI    (In a) (In b) _) = [a,b]
inPorts (ARROW (In a) _)        = [a]
inPorts (FRIN  _)               = []
inPorts (FROUT (In a))          = [a]
inPorts (T     (In a))          = [a]

outPorts :: Atom a -> [a]
outPorts (L     _ (Out b) (Out c)) = [b,c]
outPorts (FO    _ (Out b) (Out c)) = [b,c]
outPorts (FOE   _ (Out b) (Out c)) = [b,c]
outPorts (A     _ _ (Out c))       = [c]
outPorts (FI    _ _ (Out c))       = [c]
outPorts (ARROW _ (Out b))         = [b]
outPorts (FRIN  (Out a))           = [a]
outPorts (FROUT _)                 = []
outPorts (T     _)                 = []


