{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module Juvix.Backends.Graph where

import qualified Data.Graph.Inductive       as Graph
import           Data.Graph.Inductive       hiding (Node, Network, nodes)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Prelude                    (error)
import           Control.Lens

import           Juvix.Library              hiding (link, reduce, empty)
import           Juvix.NodeInterface
import           Juvix.Backends.Interface
import           Juvix.Backends.Env

data EdgeInfo = Edge (Node, PortType) (Node, PortType)
              deriving (Show, Eq)

newtype Flip p a b = Flip { runFlip :: p b a }
                   deriving (Show, Generic, Eq, Ord, Typeable)

type Net a = Gr a EdgeInfo

type FlipNet = Flip Gr EdgeInfo

-- Run Function ----------------------------------------------------------------
runFlipNet :: EnvNetInfo (FlipNet b) a → FlipNet b → InfoNet (FlipNet b)
runFlipNet f net = runNet f net
                          (toInteger (length (Graph.nodes (runFlip net))))
-- Network Instances  ----------------------------------------------------------

instance Network FlipNet where
  linkAll (RELAuxiliary0 {primary, node}) =
    linkHelper primary Prim node
  linkAll (RELAuxiliary1 {primary, node, auxiliary1}) =
    traverse_ (\ (t, nt) → linkHelper t nt node)
              [(primary, Prim), (auxiliary1, Aux1)]
  linkAll (RELAuxiliary2 {primary, node, auxiliary1, auxiliary2}) =
    traverse_ (\ (t, nt) → linkHelper t nt node)
              [(primary, Prim), (auxiliary1, Aux1), (auxiliary2, Aux2)]

  link (node1, port1) (node2, port2) =
    modify @"net" (Flip . (insEdge (node1, node2, Edge (node1, port1) (node2, port2))) . runFlip)

  -- post condition, must delete the old node passed after the set of transitions are done!
  relink (oldNode, port) new@(newNode, _newPort) = do
    edge ← findEdge (oldNode, port)
    case edge of
      Just re@(nodeToRelink, _) → modify @"net" (Flip . insEdge (nodeToRelink, newNode, Edge re new) . runFlip )
      Nothing                   → pure () -- The port was really free to begin with!

  -- | rewire is used to wire two auxiliary nodes together
  -- when the main nodes annihilate each other
  rewire (a, pa) (b, pb) = do
    edge ← findEdge (b, pb)
    traverse_ (relink (a, pa)) edge

  newNode lang = do
    net ← runFlip <$> get @"net"
    let (_,maxNum)
          | isEmpty net = (0, 0)
          | otherwise   = nodeRange net
    put @"net" (Flip $ insNode (succ maxNum, lang) net)
    pure (succ maxNum)

  delNodes xs = modify @"net" (Flip . Graph.delNodes xs . runFlip)

  deleteRewire oldNodesToDelete newNodes = do
    (Flip net) ← get @"net"
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
    (Flip net) ← get @"net"
    case fst (match n net) of
      Just context → f $ snd $ labNode' context
      Nothing      → pure Nothing

-- Helper functions ------------------------------------------------------------
linkHelper :: HasState "net" (FlipNet a) m ⇒ REL NumPort → PortType → Node → m ()
linkHelper rel nodeType node =
  case rel of
    Link (Port portType node1) → link (node, nodeType) (node1, portType)
    Link FreePort              → pure ()
    ReLink oldNode oldPort     → relink (oldNode, oldPort) (node, nodeType)


findConflict ∷ Set.Set Node → [EdgeInfo] → [((Node, PortType), (Node, PortType))]
findConflict nodes neighbors = Set.toList (foldr f mempty neighbors)
  where
    f (Edge t1 t2@(_,_)) xs =
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
-- these are made so we restrict what is needed the most by auxFromGraph
convPrim :: Prim p ⇒ (Node, PortType) → p → p
convPrim (n,Prim) con = set prim (Primary n) con
convPrim (_,_) con    = con

convAux1 :: Aux1 p ⇒ (Node, PortType) → p → p
convAux1 (n,Aux1) con = set aux1 (Auxiliary n) con
convAux1 a con        = convPrim a con

convAux2 :: Aux2 p ⇒ (Node, PortType) → p → p
convAux2 (n,Aux2) con = set aux2 (Auxiliary n) con
convAux2 a con        = convAux1 a con

convAux3 :: Aux3 p ⇒ (Node, PortType) → p → p
convAux3 (n,Aux2) con = set aux3 (Auxiliary n) con
convAux3 a con        = convAux2 a con

convAux4 :: Aux4 p ⇒ (Node, PortType) → p → p
convAux4 (n,Aux2) con = set aux4 (Auxiliary n) con
convAux4 a con        = convAux3 a con

convAux5 :: Aux5 p ⇒ (Node, PortType) → p → p
convAux5 (n,Aux2) con = set aux5 (Auxiliary n) con
convAux5 a con        = convAux4 a con

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
