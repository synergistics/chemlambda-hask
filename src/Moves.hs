{-# LANGUAGE MultiWayIf #-}
module Moves where

import Port
import Atom
import Node
import Graph
import Pattern


data NewPort a = NewId Int | ActualId a deriving ( Show )

mkActualId :: Port a -> NewPort a
mkActualId = ActualId . portId


type NewNode a = Node (NewPort a)

combMove :: (Eq a) => (Node a, Node a) -> [NewNode a]
combMove (nodeN, Node ARROW [d,e]) =
  case nodeN of
    (Node L [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        d' = mkActualId d
        e' = mkActualId e
      in 
        if | b `connects` d -> [lam a' e' c']
           | c `connects` d -> [lam a' b' e']

    (Node FO [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> [fo a' e' c']
           | c `connects` d -> [fo a' b' e']
    
    (Node FOE [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> [foe a' e' c']
           | c `connects` d -> [foe a' b' e']
    
    (Node A [a,b,_]) ->
      let
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
      in 
        [app a' b' e']
    
    (Node FI [a,b,_]) ->
      let
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
      in 
        [fi a' b' e']
    
    (Node ARROW [a,_]) ->
      let
        a' = mkActualId a
        e' = mkActualId e
      in 
        [arrow a' e']
    
    (Node FRIN [_]) ->
      let
        e' = mkActualId e
      in 
        [frin e']

betaMove :: (Node a, Node a) -> [NewNode a]
betaMove (Node L [a,b,c], Node A [d,e,f]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
  in
    [arrow a' f', arrow e' b'] 

fanInMove :: (Node a, Node a) -> [NewNode a]
fanInMove (Node FI [a,b,c], Node FOE [d,e,f]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
  in
    [arrow a' f', arrow b' e'] 

distLMove :: (Node a, Node a) -> [NewNode a]
distLMove (Node L [a,b,c], outNode) = 
  case elem (atom outNode) [FO,FOE] of 
    True -> let
        Node _ [d,e,f] = outNode
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
        f' = mkActualId f
        i  = NewId 0
        j  = NewId 1 
        k  = NewId 2 
        l  = NewId 3 
      in
        [fi j i b', lam k i e', lam l j f', foe a' k l] 

distAMove :: (Node a, Node a) -> [NewNode a]
distAMove (Node A [a,b,c], outNode) = 
  case elem (atom outNode) [FO,FOE] of 
    True -> let
        Node _ [d,e,f] = outNode
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
        f' = mkActualId f
        i  = NewId 0
        j  = NewId 1 
        k  = NewId 2 
        l  = NewId 3 
      in
        [foe a' i j, foe b' k l, app i k e', app j l f'] 

distFIMove :: (Node a, Node a) -> [NewNode a]
distFIMove (Node FI [a,b,c], Node FO [d,e,f]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
    i  = NewId 0
    j  = NewId 1 
    k  = NewId 2 
    l  = NewId 3 
  in
    [fo a' i j, fo b' k l, fi i k e', fi j l f']
    
distFOMove :: (Node a, Node a) -> [NewNode a]
distFOMove (Node FO [a,b,c], Node FOE [d,e,f]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
    i  = NewId 0
    j  = NewId 1 
    k  = NewId 2 
    l  = NewId 3 
  in
    [fi j i b', fo k i e', fo l j f', foe a' k l]

pruneMove :: Eq a => (Node a, Node a) -> [NewNode a]
pruneMove lp = case lp of 
  (Node A [a,b,c], Node T [d]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
    in [t a', t b']

  (Node FI [a,b,c], Node T [d]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
    in [t a', t b']

  (Node L [a,b,c], Node T [d]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
    in [t a', frin b']

  (Node FO [a,b,c], Node T [d]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
      c' = mkActualId c
    in 
      if | b `connects` d -> [arrow a' c']
         | c `connects` d -> [arrow a' b']

  (Node FOE [a,b,c], Node T [d]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
      c' = mkActualId c
    in 
      if | b `connects` d -> [arrow a' c']
         | c `connects` d -> [arrow a' b']
