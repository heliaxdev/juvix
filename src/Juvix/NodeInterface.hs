{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Juvix.NodeInterface where

import           Control.Lens

-- there is a probably better way of doing this, instead of writing the concrete terms
data ShellNode a
  = ShellAuxs {_prim :: a, _aux1 :: a, _aux2 :: a, _aux3 :: a, _aux4 :: a, _aux5 :: a}
  -- Need this so aux1 and aux2 need applicative instead of functor
  | ShellPrim {_prim :: a}

makeFieldsNoPrefix ''ShellNode
