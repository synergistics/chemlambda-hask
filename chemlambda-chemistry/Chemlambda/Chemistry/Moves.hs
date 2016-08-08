{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Chemlambda.Chemistry.Moves
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
combMove :: (Eq a) => Graph a -> Graph (NewId a)
combMove (nodes -> [nodeN, Node ARROW [d,e]]) =
  case nodeN of
    (Node L [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> mkGraph [lam a' e' c']
           | c `connects` d -> mkGraph [lam a' b' e']

    (Node FO [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> mkGraph [fo a' e' c']
           | c `connects` d -> mkGraph [fo a' b' e']
    
    (Node FOE [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        e' = mkActualId e
      in 
        if | b `connects` d -> mkGraph [foe a' e' c']
           | c `connects` d -> mkGraph [foe a' b' e']
    
    (Node A [a,b,_]) ->
      let
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
      in 
        mkGraph [app a' b' e']
    
    (Node FI [a,b,_]) ->
      let
        a' = mkActualId a
        b' = mkActualId b
        e' = mkActualId e
      in 
        mkGraph [fi a' b' e']
    
    (Node ARROW [a,_]) ->
      let
        a' = mkActualId a
        e' = mkActualId e
      in 
        mkGraph [arrow a' e']
    
    (Node FRIN [_]) ->
      let
        e' = mkActualId e
      in 
        mkGraph [frin e']

betaMove :: Graph a -> Graph (NewId a)
betaMove (nodes -> [Node L [a,b,c], Node A [d,e,f]]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
  in
    mkGraph [arrow a' f', arrow e' b'] 

fanInMove :: Graph a -> Graph (NewId a)
fanInMove (nodes -> [Node FI [a,b,c], Node FOE [d,e,f]]) = 
  let
    a' = mkActualId a
    b' = mkActualId b
    e' = mkActualId e
    f' = mkActualId f
  in
    mkGraph [arrow a' f', arrow b' e'] 

distLMove :: Graph a -> Graph (NewId a)
distLMove (nodes -> [Node L [a,b,c], outNode]) = 
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
        mkGraph [fi j i b', lam k i e', lam l j f', foe a' k l] 

distAMove :: Graph a -> Graph (NewId a)
distAMove (nodes -> [Node A [a,b,c], outNode]) = 
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
        mkGraph [foe a' i j, foe b' k l, app i k e', app j l f'] 

distFIMove :: Graph a -> Graph (NewId a)
distFIMove (nodes -> [Node FI [a,b,c], Node FO [d,e,f]]) = 
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
    mkGraph [fo a' i j, fo b' k l, fi i k e', fi j l f']
    
distFOMove :: Graph a -> Graph (NewId a)
distFOMove (nodes -> [Node FO [a,b,c], Node FOE [d,e,f]]) = 
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
    mkGraph [fi j i b', fo k i e', fo l j f', foe a' k l]

pruneAMove :: Graph a -> Graph (NewId a)
pruneAMove (nodes -> [Node A [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in mkGraph [t a', t b']

pruneFIMove :: Graph a -> Graph (NewId a)
pruneFIMove (nodes -> [Node FI [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in mkGraph [t a', t b']

pruneLMove :: Graph a -> Graph (NewId a)
pruneLMove (nodes -> [Node L [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in mkGraph [t a', frin b']

pruneFObMove :: Graph a -> Graph (NewId a)
pruneFObMove (nodes -> [Node FO [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    c' = mkActualId c
  in 
    mkGraph [arrow a' c']

pruneFOEbMove :: Graph a -> Graph (NewId a)
pruneFOEbMove (nodes -> [Node FOE [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    c' = mkActualId c
  in 
    mkGraph [arrow a' c']

pruneFOcMove :: Graph a -> Graph (NewId a)
pruneFOcMove (nodes -> [Node FO [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in 
    mkGraph [arrow a' b']

pruneFOEcMove :: Graph a -> Graph (NewId a)
pruneFOEcMove (nodes -> [Node FOE [a,b,c], Node T [d]]) =
  let
    a' = mkActualId a
    b' = mkActualId b
  in 
    mkGraph [arrow a' b']
