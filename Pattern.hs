module Pattern where

import Data.Maybe
import Port
import Atom
import Node
import Graph

data Pattern a b = Pattern (Graph a -> [b])

instance Functor (Pattern a) where
  fmap f p = Pattern $ \g -> map f $ match p g

instance Applicative (Pattern a) where
  pure a  = Pattern $ const [a]
  f <*> p = Pattern $ \g ->
    do
      f' <- match f g
      p' <- match p g
      return $ f' p'

instance Monad (Pattern a) where
  return  = pure
  p >>= f = Pattern $ \g -> concatMap (\m -> match (f m) g) $ match p g

match :: Pattern a b -> Graph a -> [b]
match (Pattern p) g = p g

atomOf :: Atom -> Pattern a (Node a)
atomOf a = Pattern $ \g ->
  filter (\n -> atom n == a) (nodes g)

conn ::
  (Eq a, Ord a) =>
  Atom ->
  Atom ->
  PortSel a ->
  PortSel a ->
  Pattern a (Node a, Node a)
conn a b p q = Pattern $ \g ->
  let
    pairs = do
      a' <- match (atomOf a) g
      b' <- match (atomOf b) g
      return (a',b')
  in
    filter (\(a,b) -> (isProperConn <$> p a <*> q b) == (Just True)) pairs

connNode n a p q =
  atomOf a >>= \m -> Pattern $ \g -> if (p n g == Just m) then [(n,m)] else []

-- -- conn ::
-- --   Atom ->
-- --   Atom ->
-- --   (Node a -> Maybe (Port a)) ->
-- --   (Node a -> Maybe (Port a)) ->


-- conn a1 a2 outPort inPort g =
