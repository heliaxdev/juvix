module Juvix.Backends.Michelson where

import qualified Data.Text                        as T
import           Protolude

import qualified Juvix.Backends.Michelson.Emit    as M
import qualified Juvix.Backends.Michelson.Typed   as M
import qualified Juvix.Backends.Michelson.Untyped as MU
import           Juvix.Lang
import           Juvix.Utility

transpileToMichelsonSourceFile ∷ ∀ m . (MonadError () m) ⇒ Expr → m Text
transpileToMichelsonSourceFile expr = do
  (M.SomeExpr code, paramTy, retTy, storageTy) <- transpileToMichelson expr
  return $ T.unlines [
    "parameter " <> M.emitType paramTy <> ";",
    "return " <> M.emitType retTy <> ";",
    "storage " <> M.emitType storageTy <> ";",
    "code " <> M.emitFinal code <> ";"
    ]

transpileToMichelson ∷ ∀ m . (MonadError () m) ⇒ Expr → m (M.SomeExpr, MU.Type, MU.Type, MU.Type)
transpileToMichelson _ = throw ()
