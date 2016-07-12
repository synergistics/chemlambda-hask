{-# LANGUAGE ViewPatterns #-}

module Moves where

import Port
import Atom
import Node
import Pattern
import Graph


data NewPort = New Int deriving ( Show )

type ProposedAddition a = [Node (Either NewPort a)]


combMove :: (Eq a) => (Node a, Node a) -> ProposedAddition a
combMove (nodeN, Node ARROW [d,e]) =
  case nodeN of
    (Node L [a,b,c]) -> 
      let
        a' = Right $ portId a
        b' = Right $ portId b
        c' = Right $ portId c
        e' = Right $ portId e
      in 
        if isProperConn b d 
          then [lam a' e' c']
          else [lam a' b' e']

    (Node FO [a,b,c]) -> 
      let
        a' = Right $ portId a
        b' = Right $ portId b
        c' = Right $ portId c
        e' = Right $ portId e
      in 
        if isProperConn b d 
          then [fo a' e' c']
          else [fo a' b' e']
    
    (Node FOE [a,b,c]) -> 
      let
        a' = Right $ portId a
        b' = Right $ portId b
        c' = Right $ portId c
        e' = Right $ portId e
      in 
        if isProperConn b d 
          then [foe a' e' c']
          else [foe a' b' e']
    
    (Node A [a,b,_]) ->
      let
        a' = Right $ portId a
        b' = Right $ portId b
        e' = Right $ portId e
      in 
        [app a' b' e']
    
    (Node FI [a,b,_]) ->
      let
        a' = Right $ portId a
        b' = Right $ portId b
        e' = Right $ portId e
      in 
        [fi a' b' e']
    
    (Node ARROW [a,_]) ->
      let
        a' = Right $ portId a
        e' = Right $ portId e
      in 
        [arrow a' e']
    
    (Node FRIN [_]) ->
      let
        e' = Right $ portId e
      in 
        [frin e']

betaMove :: (Node a, Node a) -> ProposedAddition a
betaMove (Node L [a,b,c], Node A [d,e,f]) = 
  let
    a' = Right $ portId a
    b' = Right $ portId b
    e' = Right $ portId e
    f' = Right $ portId f
  in
    [arrow a' f', arrow e' b'] 

fanInMove :: (Node a, Node a) -> ProposedAddition a
fanInMove (Node FI [a,b,c], Node FOE [d,e,f]) = 
  let
    a' = Right $ portId a
    b' = Right $ portId b
    e' = Right $ portId e
    f' = Right $ portId f
  in
    [arrow a' f', arrow b' e'] 

distLMove :: (Node a, Node a) -> ProposedAddition a
distLMove (Node L [a,b,c], outNode) = 
  case elem (atom outNode) [FO,FOE] of 
    True -> let
        Node _ [d,e,f] = outNode
        a' = Right $ portId a
        b' = Right $ portId b
        e' = Right $ portId e
        f' = Right $ portId f
        i  = Left $ New 0
        j  = Left $ New 1 
        k  = Left $ New 2 
        l  = Left $ New 3 
      in
        [fi j i b', lam k i e', lam l j f', foe a' k l] 

distAMove :: (Node a, Node a) -> ProposedAddition a
distAMove (Node A [a,b,c], outNode) = 
  case elem (atom outNode) [FO,FOE] of 
    True -> let
        Node _ [d,e,f] = outNode
        a' = Right $ portId a
        b' = Right $ portId b
        e' = Right $ portId e
        f' = Right $ portId f
        i  = Left $ New 0
        j  = Left $ New 1 
        k  = Left $ New 2 
        l  = Left $ New 3 
      in
        [foe a' i j, foe b' k l, app i k e', app j l f'] 

distFIMove :: (Node a, Node a) -> ProposedAddition a
distFIMove (Node FI [a,b,c], Node FO [d,e,f]) = 
  let
    a' = Right $ portId a
    b' = Right $ portId b
    e' = Right $ portId e
    f' = Right $ portId f
    i  = Left $ New 0
    j  = Left $ New 1 
    k  = Left $ New 2 
    l  = Left $ New 3 
  in
    [fo a' i j, fo b' k l, fi i k e', fi j l f']
    
distFOMove :: (Node a, Node a) -> ProposedAddition a
distFOMove (Node FO [a,b,c], Node FOE [d,e,f]) = 
  let
    a' = Right $ portId a
    b' = Right $ portId b
    e' = Right $ portId e
    f' = Right $ portId f
    i  = Left $ New 0
    j  = Left $ New 1 
    k  = Left $ New 2 
    l  = Left $ New 3 
  in
    [fi j i b', fo k i e', fo l j f', foe a' k l]

pruneMove :: Eq a => (Node a, Node a) -> ProposedAddition a
pruneMove lp = case lp of 
  (Node A [a,b,c], Node T [d]) ->  
    let
      a' = Right $ portId a
      b' = Right $ portId b
    in [t a', t b']

  (Node FI [a,b,c], Node T [d]) ->  
    let
      a' = Right $ portId a
      b' = Right $ portId b
    in [t a', t b']

  (Node L [a,b,c], Node T [d]) ->  
    let
      a' = Right $ portId a
      b' = Right $ portId b
    in [t a', frin b']

  (Node FO [a,b,c], Node T [d]) ->  
    let
      a' = Right $ portId a
      b' = Right $ portId b
      c' = Right $ portId c
    in 
      if isProperConn b d 
        then [arrow a' c']
        else [arrow a' b']

  (Node FOE [a,b,c], Node T [d]) ->  
    let
      a' = Right $ portId a
      b' = Right $ portId b
      c' = Right $ portId c
    in 
      if isProperConn b d 
        then [arrow a' c']
        else [arrow a' b']



