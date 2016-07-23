{-# LANGUAGE MultiWayIf #-}

module Chemlambda.Standard.Moves
  ( betaMove
  , combMove
  , fanInMove
  , distLMove
  , distAMove
  , distFIMove
  , distFOMove
  , pruneAMove
  , pruneFIMove
  , pruneLMove
  , pruneFObMove
  , pruneFOEbMove
  , pruneFOcMove
  , pruneFOEcMove
  )
  where

import Chemlambda.Core.Port
import Chemlambda.Core.Atom
import Chemlambda.Core.Node
import Chemlambda.Core.Graph

  
-- Might need to split
combMove :: (Eq a) => Graph [Node a] -> Graph [Node (NewId a)]
combMove (Graph [nodeN, Node ARROW [d,e]]) =
  case nodeN of
    (Node L [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> Graph [lam a' e' c']
           | c `connects` d -> Graph [lam a' b' e']

    (Node FO [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> Graph [fo a' e' c']
           | c `connects` d -> Graph [fo a' b' e']
    
    (Node FOE [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> Graph [foe a' e' c']
           | c `connects` d -> Graph [foe a' b' e']
    
    (Node A [a,b,_]) ->
      let
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
      in 
        Graph [app a' b' e']
    
    (Node FI [a,b,_]) ->
      let
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
      in 
        Graph [fi a' b' e']
    
    (Node ARROW [a,_]) ->
      let
        a' = mkActualId a
        e' = mkActualId e
      in 
        Graph [arrow a' e']
    
    (Node FRIN [_]) ->
      let
        e' = mkActualId e
      in 
        Graph [frin e']

betaMove :: Graph [Node a] -> Graph [Node (NewId a)]
betaMove (Graph [Node L [a,b,c], Node A [d,e,f]]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
  in
    Graph [arrow a' f', arrow e' b'] 

fanInMove :: Graph [Node a] -> Graph [Node (NewId a)]
fanInMove (Graph [Node FI [a,b,c], Node FOE [d,e,f]]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
  in
    Graph [arrow a' f', arrow b' e'] 

distLMove :: Graph [Node a] -> Graph [Node (NewId a)]
distLMove (Graph [Node L [a,b,c], outNode]) = 
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
        Graph [fi j i b', lam k i e', lam l j f', foe a' k l] 

distAMove :: Graph [Node a] -> Graph [Node (NewId a)]
distAMove (Graph [Node A [a,b,c], outNode]) = 
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
        Graph [foe a' i j, foe b' k l, app i k e', app j l f'] 

distFIMove :: Graph [Node a] -> Graph [Node (NewId a)]
distFIMove (Graph [Node FI [a,b,c], Node FO [d,e,f]]) = 
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
    Graph [fo a' i j, fo b' k l, fi i k e', fi j l f']
    
distFOMove :: Graph [Node a] -> Graph [Node (NewId a)]
distFOMove (Graph [Node FO [a,b,c], Node FOE [d,e,f]]) = 
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
    Graph [fi j i b', fo k i e', fo l j f', foe a' k l]

pruneAMove :: Graph [Node a] -> Graph [Node (NewId a)]
pruneAMove (Graph [Node A [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in Graph [t a', t b']

pruneFIMove :: Graph [Node a] -> Graph [Node (NewId a)]
pruneFIMove (Graph [Node FI [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in Graph [t a', t b']

pruneLMove :: Graph [Node a] -> Graph [Node (NewId a)]
pruneLMove (Graph [Node L [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in Graph [t a', frin b']

pruneFObMove :: Graph [Node a] -> Graph [Node (NewId a)]
pruneFObMove (Graph [Node FO [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    c' = mkActualId c
  in 
    Graph [arrow a' c']

pruneFOEbMove :: Graph [Node a] -> Graph [Node (NewId a)]
pruneFOEbMove (Graph [Node FOE [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    c' = mkActualId c
  in 
    Graph [arrow a' c']

pruneFOcMove :: Graph [Node a] -> Graph [Node (NewId a)]
pruneFOcMove (Graph [Node FO [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in 
    Graph [arrow a' b']

pruneFOEcMove :: Graph [Node a] -> Graph [Node (NewId a)]
pruneFOEcMove (Graph [Node FOE [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in 
    Graph [arrow a' b']


