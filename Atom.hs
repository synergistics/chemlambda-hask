module Atom where

import Port


class Atom a where


data L a = L (In a) (Out a) (Out a)  deriving ( Show )
instance Atom (L a) where
lam :: a -> a -> a -> L a
lam a b c = L (In a) (Out b) (Out c)


data FO a = FO (In a) (Out a) (Out a) deriving ( Show )
instance Atom (FO a) where
fo :: a -> a -> a -> FO a
fo a b c = FO (In a) (Out b) (Out c)


data FOE a = FOE (In a) (Out a) (Out a) deriving ( Show )
instance Atom (FOE a) where
foe :: a -> a -> a -> FOE a
foe a b c = FOE (In a) (Out b) (Out c)


data A a = A (In a) (In a) (Out a) deriving ( Show )
instance Atom (A a) where
app :: a -> a -> a -> A a
app a b c = A (In a) (In b) (Out c)


data FI a = FI (In a) (In a) (Out a)  deriving ( Show )
instance Atom (FI a) where
fi :: a -> a -> a -> FI a
fi a b c = FI (In a) (In b) (Out c)


data ARROW a = ARROW (In a) (Out a)  deriving ( Show )
instance Atom (ARROW a) where
arrow :: a -> a -> ARROW a
arrow a b = ARROW (In a) (Out b)


data FRIN a = FRIN (Out a)  deriving ( Show )
instance Atom (FRIN a) where
frin :: a -> FRIN a
frin a = FRIN (Out a)


data FROUT a = FROUT (In a)  deriving ( Show )
instance Atom (FROUT a) where
frout :: a -> FROUT a
frout a = FROUT (In a)



