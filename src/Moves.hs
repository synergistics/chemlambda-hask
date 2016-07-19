{-# LANGUAGE MultiWayIf #-}
module Moves where
import Port
import Atom
import Node
import Graph
import Pattern


mkActualId :: Port a -> NewId a
mkActualId = ActualId . portId


combMove :: (Eq a) => Graph [Node a] -> Graph [Node (NewId a)]
combMove (Graph [nodeN, Node ARROW [d,e]]) =
  case nodeN of
    (Node L [a,b,c]) -> 
      let
        a' = mkActualId a
        b' = mkActualId b
        c' = mkActualId c
        d' = mkActualId d
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

pruneMove :: Eq a => Graph [Node a] -> Graph [Node (NewId a)]
pruneMove lp = case lp of 
  (Graph [Node A [a,b,c], Node T [d]]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
    in Graph [t a', t b']

  (Graph [Node FI [a,b,c], Node T [d]]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
    in Graph [t a', t b']

  (Graph [Node L [a,b,c], Node T [d]]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
    in Graph [t a', frin b']

  (Graph [Node FO [a,b,c], Node T [d]]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
      c' = mkActualId c
    in 
      if | b `connects` d -> Graph [arrow a' c']
         | c `connects` d -> Graph [arrow a' b']

  (Graph [Node FOE [a,b,c], Node T [d]]) ->  
    let
      a' = mkActualId a
      b' = mkActualId b
      c' = mkActualId c
    in 
      if | b `connects` d -> Graph [arrow a' c']
         | c `connects` d -> Graph [arrow a' b']
