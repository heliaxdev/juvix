{-# LANGUAGE TemplateHaskell #-}

module Juvix.Backends.Maps where

import qualified Data.EnumMap.Strict as Map
import           Control.Lens
import qualified Data.Set            as Set

import           Juvix.Utility.Sugar
import           Juvix.Library            hiding (empty, link)
import           Juvix.Backends.Interface
import           Juvix.Backends.Env
import           Juvix.NodeInterface

newtype Net a = Net {ofNet :: Map.EnumMap Node (NodeInfo a)}
              deriving Show

data NodeInfo a = NInfo { _typ     :: a
                        , _edges   :: Map.EnumMap PortType (Node, PortType)
                        } deriving (Show)

makeLenses ''NodeInfo

-- Run Function ----------------------------------------------------------------
runMapNet :: EnvNetInfo (Net b) a → Net b → InfoNet (Net b)
runMapNet f net = runNet f net (toInteger (length (ofNet net)))
-- Network Instances  ----------------------------------------------------------

-- TODO :: Bug in this implementation makes this not work, fix it!
instance Network Net where
  link np1@(node1, port1) np2@(node2, port2) = do
    Net net ← get @"net"
    case (Map.lookup node1 net, Map.lookup node2 net) of
      (Just n1, Just n2) →
        let newN1 = over edges (Map.insert port1 np2) n1
            newN2 = over edges (Map.insert port2 np1) n2
        in put @"net" $ Net (Map.insert node2 newN2
                            $ Map.insert node1 newN1 net)
      _ → pure ()

  empty = Net Map.empty

  isBothPrimary n =
    findEdge (n, Prim) >>| \case
      Just (_,Prim) → True
      Just (_,_)    → False
      Nothing       → False

  newNode lang = do
    Net net ← get @"net"
    let num
          | Map.null net = 0
          | otherwise    = fst (Map.findMax net)
    put @"net" (Net (Map.insert (succ num) (NInfo lang Map.empty) net))
    pure (succ num)

  nodes = fmap fst . Map.toList . ofNet <$> get @"net"

  findEdge (node, nodePort) = do
    Net net ← get @"net"
    pure $ do
     n ← Map.lookup node net
     Map.lookup nodePort (n^.edges)

  -- Note this does not remove all edges to the deleted node
  deleteEdge (n1, p1) (n2, p2) =
    modify @"net" (Net
                  . Map.adjust (over edges (Map.delete p1)) n1
                  . Map.adjust (over edges (Map.delete p2)) n2
                  . ofNet)

  delNodes xs = do
    Net net ← get @"net"
    let delEdges node net
          = foldr (flip deleteAllPoints) net (Map.toList (node^.edges))
        delNodeAndEdges
          = foldr (\nodeNum net →
                     case Map.lookup nodeNum net of
                       Just node → Map.delete nodeNum (delEdges node net)
                       Nothing   → net)
    put @"net" (Net (delNodeAndEdges net xs))

  deleteRewire oldNodesToDelete newNodes = do
    Net net ← get @"net"
    let newNodeSet           = Set.fromList newNodes
        neighbor             = neighbors oldNodesToDelete net
        conflictingNeighbors = findConflict newNodeSet neighbor
    traverse_ (uncurry link) conflictingNeighbors
    delNodes oldNodesToDelete

deleteAllPoints :: (Foldable t, Enum k)
                ⇒ Map.EnumMap k (NodeInfo a)
                → t (k, PortType)
                → Map.EnumMap k (NodeInfo a)
deleteAllPoints = foldr f
  where
    f (n, pt) = Map.adjust (over edges (Map.delete pt)) n

neighbors :: [Node] → Map.EnumMap Node (NodeInfo a) → [EdgeInfo]
neighbors oldNodes net = do
  node ← oldNodes
  case Map.lookup node net of
    Nothing → []
    Just x  → do
      (port, neigbhors) ← Map.toList (x^.edges)
      pure (Edge neigbhors (node, port))


instance DifferentRep Net where
  aux0FromGraph con = auxFromGraph convPrim (con Free)
  aux1FromGraph con = auxFromGraph convAux1 (con Free FreeNode)
  aux2FromGraph con = auxFromGraph convAux2 (con Free FreeNode FreeNode)
  aux3FromGraph con = auxFromGraph convAux3 (con Free FreeNode FreeNode FreeNode)
  aux4FromGraph con = auxFromGraph convAux4 (con Free FreeNode FreeNode FreeNode FreeNode)
  aux5FromGraph con = auxFromGraph convAux5 (con Free FreeNode FreeNode FreeNode FreeNode FreeNode)
  langToPort n f = do
    Net net ← get @"net"
    case Map.lookup n net of
      Just context → f (context^.typ)
      Nothing      → pure Nothing

-- Graph to more typed construction Helper --------------------------------------

auxFromGraph :: HasState "net" (Net a) m
             ⇒ ((Node, PortType) -> b -> b)
             → b
             → Node
             → m (Maybe b)
auxFromGraph conv constructor num = do
  Net net ← get @"net"
  let edges = neighbors [num] net
  case edges of
    []     → pure Nothing
    (_:_)  → pure $ Just $ foldr f constructor edges
  where
    f (Edge (n1, n1port) (n2, n2port)) con
      | n1 == num = conv (n2, n1port) con
      | n2 == num = conv (n1, n2port) con
      | otherwise = con
