{-# LANGUAGE TemplateHaskell #-}

module Juvix.Backends.Maps where

import qualified Data.EnumMap.Strict as Map
import           Control.Lens
import qualified Data.Set            as Set

import           Juvix.Utility.Sugar
import           Juvix.Library            hiding (empty)
import           Juvix.Backends.Interface
import           Juvix.Backends.Env
import           Juvix.NodeInterface

newtype Net a = Net {ofNet :: Map.EnumMap Node (NodeInfo a)}
              deriving Show

data NodeInfo a = NInfo { _typ     :: a
                        , _edges   :: Map.EnumMap PortType (Node, PortType)
                        } deriving (Show)

makeLenses ''NodeInfo

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
          = foldr (\nodeNum net → case Map.lookup nodeNum net of
                      Just node → Map.delete nodeNum (delEdges node net)
                      Nothing   → net)
    put @"net" (Net (delNodeAndEdges net xs))

deleteAllPoints :: (Foldable t, Enum k)
                ⇒ Map.EnumMap k (NodeInfo a)
                → t (k, PortType)
                → Map.EnumMap k (NodeInfo a)
deleteAllPoints = foldr f
  where
    f (n, pt) = Map.adjust (over edges (Map.delete pt)) n


findConflict nodes neighbors = Set.toList (foldr f mempty neighbors)
  where
    f = undefined
