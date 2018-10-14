module Juvix.Lang.Expr where

import qualified Data.Text       as T
import           Protolude

import qualified IRTS.Lang       as I

import           Juvix.Lang.Core
import           Juvix.Utility

type Expr = I.LExp

instance PrettyPrint Expr where
  prettyPrintValue = \case
    I.LV n -> show n
    I.LApp _ func args -> prettyPrintValue func <> T.intercalate " " (fmap prettyPrintValue args)

type Decl = I.LDecl

instance PrettyPrint Decl where
  prettyPrintValue = \case
    I.LFun _ name args expr -> prettyPrintValue name <> " = \\" <> T.intercalate " " (fmap prettyPrintValue args) <> " -> " <> prettyPrintValue expr
