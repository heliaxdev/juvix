module Juvix.Lang.Core where

import qualified Data.Text     as T
import           Protolude     hiding (Alt (..), Const (..))

import qualified Idris.Core.TT as I
import qualified IRTS.Lang     as I

import           Juvix.Utility

type Term a = I.TT a

type Name = I.Name

type Expr = I.LExp

type Alt = I.LAlt

type Const = I.Const

type Prim = I.PrimFn

type Decl = I.LDecl

{- -}

instance PrettyPrint Name

instance PrettyPrint Expr where
  prettyPrintValue = \case
    I.LV name             -> prettyPrintValue name
    I.LApp _ func args    -> prettyPrintValue func <> T.intercalate " " (fmap prettyPrintValue args)
    I.LLazyApp func args  -> prettyPrintValue func <> " ~ " <> T.intercalate " " (fmap prettyPrintValue args)
    I.LLazyExp expr       -> "~" <> prettyPrintValue expr
    I.LForce expr         -> "!" <> prettyPrintValue expr
    I.LLet name bind expr -> "let " <> prettyPrintValue name <> " = " <> prettyPrintValue bind <> " in " <> prettyPrintValue expr
    I.LLam args body      -> "\\" <> T.intercalate " " (fmap prettyPrintValue args) <> " -> " <> prettyPrintValue body
    I.LProj expr num      -> "proj " <> prettyPrintValue expr <> " " <> prettyPrintValue num
    I.LCon _ _ name args  -> prettyPrintValue name <> " " <> T.intercalate " " (fmap prettyPrintValue args)
    I.LCase ct expr alts  -> "case " <> prettyPrintValue expr <> " of " <> T.intercalate "; " (fmap prettyPrintValue alts)
    I.LConst const        -> prettyPrintValue const
    I.LOp prim exprs      -> prettyPrintValue prim <> " " <> T.intercalate " " (fmap prettyPrintValue exprs)
    I.LNothing            -> "nothing"
    I.LError str          -> "error: " <> show str

instance PrettyPrint Alt where
  prettyPrintValue = \case
    I.LDefaultCase expr -> "default -> " <> prettyPrintValue expr
    I.LConstCase const expr -> prettyPrintValue const <> " -> " <> prettyPrintValue expr
    I.LConCase _ con args expr -> prettyPrintValue con <> " " <> T.intercalate " " (fmap prettyPrintValue args) <> " -> " <> prettyPrintValue expr

instance PrettyPrint Const

instance PrettyPrint Prim

instance PrettyPrint Decl where
  prettyPrintValue = \case
    I.LFun _ name args expr -> prettyPrintValue name <> " = \\" <> T.intercalate " " (fmap prettyPrintValue args) <> " -> " <> prettyPrintValue expr
