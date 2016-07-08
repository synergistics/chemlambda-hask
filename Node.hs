module Node where

import Atom


data Node a = Node 
  { atom  :: Atom
  , ports :: [a] }
  deriving ( Show, Eq )

-- Smart constructors
lam :: a -> a -> a -> Node a
lam a b c = Node L [a,b,c]

fo :: a -> a -> a -> Node a
fo a b c = Node FO [a,b,c] 

foe :: a -> a -> a -> Node a
foe a b c = Node FOE [a,b,c]

app :: a -> a -> a -> Node a
app a b c = Node A [a,b,c]

fi :: a -> a -> a -> Node a
fi a b c = Node FI [a,b,c]

arrow :: a -> a -> Node a
arrow a b = Node ARROW [a,b]

frin :: a -> Node a
frin a = Node FRIN [a] 

frout :: a -> Node a
frout a = Node FROUT [a]

t :: a -> Node a
t a = Node T [a] 

-- Selectors
liPort :: Node a -> Maybe a
liPort n = if elem (atom n) [A,FI] 
  then Just $ ports n !! 0 
  else Nothing

riPort :: Node a -> Maybe a
riPort n = if elem (atom n) [A,FI] 
  then Just $ ports n !! 1 
  else Nothing

miPort :: Node a -> Maybe a
miPort n = if elem (atom n) [L,FO,FOE,ARROW,FROUT,T] 
  then Just $ ports n !! 0 
  else Nothing

loPort :: Node a -> Maybe a
loPort n = if elem (atom n) [L,FO,FOE] 
  then Just $ ports n !! 1 
  else Nothing

roPort :: Node a -> Maybe a
roPort n = if elem (atom n) [L,FO,FOE] 
  then Just $ ports n !! 2 
  else Nothing

moPort :: Node a -> Maybe a
moPort n = if elem (atom n) [A,FI,ARROW,FRIN] 
  then Just $ last $ ports n 
  else Nothing

