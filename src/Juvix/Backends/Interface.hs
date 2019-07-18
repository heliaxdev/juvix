{-# LANGUAGE ConstraintKinds #-}

module Juvix.Backends.Interface where

import Juvix.Library
import Juvix.NodeInterface

type Node = Int

type NetState net m = HasState "net" net m

data PortType = Prim
              | Aux1
              | Aux2
              | Aux3
              | Aux4
              | Aux5
              deriving (Ord,Eq, Show)

data NumPort = Port PortType Node
             | FreePort
             deriving Show

-- Rewrite REL into tagless final, so we aren't memory
-- wasting on this silly tag, just pass in the function!
-- | REL: a type that displays whether we are linking from an old node or just adding a new link
data REL a = Link a
           | ReLink Node PortType
           deriving (Show)

-- | Type for specifying how one wants to link nodes
data Relink
  = RELAuxiliary2 { node       :: Node
                  , primary    :: REL NumPort
                  , auxiliary1 :: REL NumPort
                  , auxiliary2 :: REL NumPort }
  | RELAuxiliary1 { node :: Node, primary :: REL NumPort, auxiliary1 :: REL NumPort }
  | RELAuxiliary0 { node :: Node, primary :: REL NumPort }
  deriving (Show)

class Network net where
  isBothPrimary :: NetState (net a) m ⇒ Node → m Bool
  linkAll       :: NetState (net a) m ⇒ Relink → m ()
  link          :: NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
  relink        :: NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
  rewire        :: NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
  newNode       :: NetState (net a) m ⇒ a → m Node
  delNodes      :: NetState (net a) m ⇒ [Node] → m ()
  deleteRewire  :: NetState (net a) m ⇒ [Node] → [Node] → m ()
  deleteEdge    :: NetState (net a) m ⇒ (Node, PortType) → (Node, PortType) → m ()
  nodes         :: NetState (net a) m ⇒ m [Node]
  empty             :: net a
  findEdge      ∷ NetState (net a) m ⇒ (Node, PortType) → m (Maybe (Node, PortType))

-- | a network that has one type for nodes but another for
-- actually doing computation on the node
class Network net ⇒ DifferentRep net where
  aux0FromGraph :: (NetState (net a) m, Prim b)
                ⇒ (Primary → b)
                → Node
                → m (Maybe b)
  aux1FromGraph :: (NetState (net a) m, Aux1 b)
                ⇒ (Primary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux2FromGraph :: (NetState (net a) m, Aux2 b)
                ⇒ (Primary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux3FromGraph :: (NetState (net a) m, Aux3 b)
                ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux4FromGraph :: (NetState (net a) m, Aux4 b)
                ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  aux5FromGraph :: (NetState (net a) m, Aux5 b)
                ⇒ (Primary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → Auxiliary → b)
                → Node
                → m (Maybe b)
  langToPort :: NetState (net a) m ⇒ Node → (a → m (Maybe b)) → m (Maybe b)
