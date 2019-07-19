{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module Juvix.Backends.Graph where

import qualified Data.Graph.Inductive       as Graph
import           Data.Graph.Inductive       hiding (Node, Network, nodes)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Prelude                    (error)

import           Juvix.Library              hiding (link, reduce, empty)
import           Juvix.NodeInterface
import           Juvix.Backends.Interface
import           Juvix.Backends.Env
import           Juvix.Utility.Helper

data EdgeInfo = Edge (Node, PortType) (Node, PortType)
              deriving (Show, Eq)

type Net a   = Gr a EdgeInfo
type FlipNet = Flip Gr EdgeInfo

-- Run Function ----------------------------------------------------------------
runFlipNet :: EnvNetInfo (FlipNet b) a → FlipNet b → InfoNet (FlipNet b)
runFlipNet f net = runNet f net
                          (toInteger (length (Graph.nodes (runFlip net))))
-- Network Instances  ----------------------------------------------------------

instance Network FlipNet where
  link (node1, port1) (node2, port2) =
    let edgeInfo = (node1, node2, Edge (node1, port1) (node2, port2)) in
      modify @"net" (Flip . (insEdge edgeInfo) . runFlip)

  newNode lang = do
    net ← runFlip <$> get @"net"
    let (_,maxNum)
          | isEmpty net = (0, 0)
          | otherwise   = nodeRange net
    put @"net" (Flip $ insNode (succ maxNum, lang) net)
    pure (succ maxNum)

  delNodes xs = modify @"net" (Flip . Graph.delNodes xs . runFlip)

  deleteRewire oldNodesToDelete newNodes = do
    Flip net ← get @"net"
    let newNodeSet           = Set.fromList newNodes
        neighbors            = fst <$> (oldNodesToDelete >>= lneighbors net)
        conflictingNeighbors = findConflict newNodeSet neighbors
    traverse_ (uncurry link) conflictingNeighbors
    modify @"net" (Flip . Graph.delNodes oldNodesToDelete . runFlip)

  deleteEdge t1@(n1,_) t2@(n2,_)
    = modify @"net" (Flip
                    . delAllLEdge (n1, n2, (Edge t1 t2))
                    . delAllLEdge (n2, n1, (Edge t2 t1))
                    . runFlip)

  isBothPrimary node = do
    net ← runFlip <$> get @"net"
    pure $ null
         $ filter (\ (Edge (_, p) (_, p')) → p == Prim && p' == Prim)
         $ fmap fst
         $ lneighbors net node

  nodes = Graph.nodes . runFlip <$> get @"net"

  empty = Flip Graph.empty

  findEdge (node, port) = do
    net ← runFlip <$> get @"net"
    pure (fmap other $ headMay $ filter f $ lneighbors net node)
    where
      f (Edge t1 t2, _)
        | t1 == (node, port) = True
        | t2 == (node, port) = True
        | otherwise          = False
      other (Edge t1 t2, _)
        | t1 == (node, port) = t2
        | t2 == (node, port) = t1
        | otherwise          = error "doesn't happen"


instance DifferentRep FlipNet where
  aux0FromGraph con = auxFromGraph convPrim (con Free)
  aux1FromGraph con = auxFromGraph convAux1 (con Free FreeNode)
  aux2FromGraph con = auxFromGraph convAux2 (con Free FreeNode FreeNode)
  aux3FromGraph con = auxFromGraph convAux3 (con Free FreeNode FreeNode FreeNode)
  aux4FromGraph con = auxFromGraph convAux4 (con Free FreeNode FreeNode FreeNode FreeNode)
  aux5FromGraph con = auxFromGraph convAux5 (con Free FreeNode FreeNode FreeNode FreeNode FreeNode)
  langToPort n f = do
    Flip net ← get @"net"
    case fst (match n net) of
      Just context → f $ snd $ labNode' context
      Nothing      → pure Nothing

-- Helper functions ------------------------------------------------------------
findConflict ∷ Set.Set Node → [EdgeInfo] → [((Node, PortType), (Node, PortType))]
findConflict nodes neighbors = Set.toList (foldr f mempty neighbors)
  where
    f (Edge t1 t2) xs =
      case (makeMap Map.!? t2, makeMap Map.!? t1) of
        (Just m2, Just m1) → Set.insert (m2, m1) xs
        _                  → xs
    makeMap = foldr f mempty neighbors
      where
        f (Edge t1@(n1,_) t2@(n2,_)) hash
          | Set.member n1 nodes = Map.insert t2 t1 hash
          | Set.member n2 nodes = Map.insert t1 t2 hash
          | otherwise           = hash

-- Graph to more typed construction Helper --------------------------------------

auxFromGraph :: (HasState "net" (FlipNet a)  m)
             ⇒ ((Node, PortType) → b → b)
             → b
             → Node
             → m (Maybe b)
auxFromGraph conv constructor num =
  fmap (foldr f constructor . lneighbors') . fst . match num . runFlip <$> get @"net"
  where
    f (Edge (n1, n1port) (n2, n2port), _) con
      | n1 == num = conv (n2, n1port) con
      | n2 == num = conv (n1, n2port) con
      | otherwise = con
