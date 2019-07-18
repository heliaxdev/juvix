{-# LANGUAGE TemplateHaskell #-}

module Juvix.Backends.Maps where

import qualified Data.EnumMap.Strict as Map

import           Juvix.Library
import           Juvix.Backends.Interface
import           Juvix.Backends.Env
import           Juvix.NodeInterface
import           Control.Lens

--Map.EnumMap

newtype Net a = Net {runNet :: Map.EnumMap Node (NodeInfo a)}

data NodeInfo a = Info { _typ     :: a
                       , _edges   :: Map.EnumMap PortType Node
                       } deriving (Show)

makeLenses ''NodeInfo

instance Network Net where
  link (node1, port1) (node2, port2) = do
    Net net ← get @"net"
    case (Map.lookup node1 net, Map.lookup node2 net) of
      (Just n1, Just n2) →
        let newN1 = over edges (Map.insert port1 node2) n1
            newN2 = over edges (Map.insert port2 node1) n2
        in put @"net" $ Net (Map.insert node2 newN2
                            $ Map.insert node1 newN1 net)
      _ → pure ()
