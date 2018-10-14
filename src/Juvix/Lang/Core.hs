module Juvix.Lang.Core where

import           Protolude

import qualified Idris.Core.TT as I

import           Juvix.Utility

type Term a = I.TT a

type Name = I.Name

instance PrettyPrint Name
