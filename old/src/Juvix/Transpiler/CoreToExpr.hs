module Juvix.Transpiler.CoreToExpr (
  coreToExpr,
  resolveVars
) where

import           Control.Monad.RWS.Strict
import qualified Data.Map                     as M
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Foundation

import qualified Juvix.Backends.Michelson     as M
import           Juvix.Core
import           Juvix.Core.CompilerTypes
import qualified Juvix.Core.GHC               as GHC
import           Juvix.Transpiler.CoreToType
import           Juvix.Transpiler.PrettyPrint
import           Juvix.Transpiler.TypeToType
import           Juvix.Transpiler.Utility

{-  Stage 1: GHC Core → Intermediary Expression

    Transform GHC Core expression into our intermediary stage expression type, executing the following transformations:

    - Eliminating unnecessary type annotations (the input GHC Core expression is assumed to be well-typed, and GHC types are erased at runtime)
    - Rewriting polymorphic calls to the appropriate typeclass instantation (Juvix does not support runtime polymorphism)
    - Miscellaneous GHC builtin replacements
-}

resolveVars ∷ GHC.CoreExpr → CompilerM GHC.CoreExpr M.Type
resolveVars expr =
  case expr of
    GHC.Coercion c -> return (GHC.Coercion c)
    GHC.Tick t e -> GHC.Tick t <$> resolveVars e
    GHC.Type t -> return (GHC.Type t)
    GHC.Lit l -> return (GHC.Lit l)
    GHC.Case e b t alts -> do
      e <- resolveVars e
      alts <- mapM (\(x, y, e) -> resolveVars e >>| \e -> (x, y, e)) alts
      return (GHC.Case e b t alts)
    GHC.App x y -> GHC.App <$> resolveVars x <*> resolveVars y
    GHC.Lam v e -> GHC.Lam v <$> resolveVars e
    GHC.Let b e -> GHC.Let b <$> resolveVars e
    GHC.Cast e t -> flip GHC.Cast t <$> resolveVars e
    GHC.Var v -> do
      let name = prettyPrintValue v
      if nameToTextSimple (GHC.varName v) == "rewrite" then return expr else do
        env ← envExprs |<< ask
        case M.lookup name env of
          Just e  -> resolveVars e
          Nothing -> return expr

coreToExpr ∷ GHC.CoreExpr → CompilerM (Expr M.Type) M.Type
coreToExpr expr = do

  let tellReturn ∷ Expr M.Type → CompilerM (Expr M.Type) M.Type
      tellReturn ret  = tell [CoreToExpr expr ret] >> return ret

      annReturn ∷ Expr M.Type → CompilerM (Expr M.Type) M.Type
      annReturn ret = do
        ty ← coreToType expr
        return (Ann ret ty)

      error ∷ ∀ a . PrettyPrint a ⇒ a → CompilerM (Expr M.Type) M.Type
      error v         = throw (NotYetImplemented (T.concat ["coreToExpr: ", prettyPrintValue v]))

  tell [Custom (T.concat ["Scoped: ", prettyPrintValue expr, " ∷ ", prettyPrintValue (GHC.exprType expr)])]

  case expr of

    {- Variables: replace builtins, replace from env, translate. -}
    GHC.Var v → do
      if GHC.isId v then do
        case nameToTextSimple (GHC.varName v) of
          "()"    → return (Lit LUnit)
          "(,)"   → return (BuiltIn "ConsPairUT")
          "(#,#)" → return (BuiltIn "ConsPairUT")
          _     → do
            let name = prettyPrintValue v
            env ← envExprs |<< ask
            case M.lookup name env of
              Just e  → coreToExpr e
              Nothing → do
                if GHC.isGlobalId v then do
                  case unForAll (GHC.varType v) of
                    typ@(GHC.TyConApp tyCon bs) | GHC.isAlgTyCon tyCon → do
                      pack ← constructorPack tyCon (GHC.varName v) bs `catch` (\_ → throw (NotYetImplemented ("pack: " `T.append` prettyPrintValue expr `T.append` " @ " `T.append` prettyPrintValue (GHC.exprType expr))))
                      ann  ← typeToType typ `catch` (\_ → throw (NotYetImplemented ("packType: " `T.append` prettyPrintValue expr)))
                      return (Ann pack ann)
                      --throw (NotYetImplemented (T.concat ["packed ADT: ", prettyPrintValue v, " for ", prettyPrintValue tyCon, " ⇒ ", prettyPrintValue pack]))
                    _ → throw (NotYetImplemented (T.concat ["coreToExpr (var): ", prettyPrintValue expr, " @ ", prettyPrintValue (GHC.varType v)]))
                else return (Var name)
        else throw (NotYetImplemented (prettyPrintValue v))

    {- Convert literals. -}
    GHC.Lit l ->
      case l of
        GHC.LitInteger i _ → return (Lit (LInt i))
        _                  → error l

    {- Replace builtins. -}
    GHC.App (GHC.App (GHC.App (GHC.App (GHC.App (GHC.Var m) _) _) _) _) (GHC.App _ (GHC.Lit (GHC.MachStr s))) | nameToTextSimple (GHC.varName m) == "rewrite" →
      tellReturn (BuiltIn (T.decodeUtf8 s))

    GHC.App (GHC.App (GHC.App (GHC.App (GHC.Var m) _) _) _) (GHC.App _ (GHC.Lit (GHC.MachStr s))) | nameToTextSimple (GHC.varName m) == "rewrite" →
      tellReturn (BuiltIn (T.decodeUtf8 s))

    GHC.App (GHC.App (GHC.App (GHC.Var m) _) _) (GHC.App _ (GHC.Lit (GHC.MachStr s))) | nameToTextSimple (GHC.varName m) == "rewrite" →
      tellReturn (BuiltIn (T.decodeUtf8 s))

    {- Type application -}
    GHC.App x (GHC.Type t) → do
      coreToExpr (tyApp x t)

    {- Discard type application TODO -}
    GHC.App x (GHC.Var v) | GHC.isTyVar v || GHC.isTcTyVar v → do
      _ ← throw (NotYetImplemented (T.concat ["App ", prettyPrintValue x, " to ", prettyPrintValue v]))
      coreToExpr x

    {- Function application. -}
    GHC.App x y → do
      x ← coreToExpr x
      y ← coreToExpr y
      annReturn (App x y)

    {- Translate value lambdas. -}
    GHC.Lam v e | GHC.isId v → do
      Lam (prettyPrintValue v) |<< coreToExpr e

    {- Discard type lambdas. -}
    GHC.Lam v e | GHC.isTyVar v → do
      coreToExpr e

    {- Not supported. -}
    GHC.Lam v _ → error v

    {- Translate non-recursive let bindings. -}
    GHC.Let (GHC.NonRec v b) e → do
      b ← coreToExpr b
      e ← coreToExpr e
      return (Let (prettyPrintValue v) b e)

    {- Not supported. -}
    GHC.Let b _ → error b

    {- Typecast or rebind; we don't need to do anything here. -}
    GHC.Case e _ _ [] → coreToExpr e

    {- Cast with irrefutable case and no binds; just evaluate the case. -}
    GHC.Case _ _ _ [(_, [], expr)] →
      coreToExpr expr

    {- Standard case expression. -}
    GHC.Case e b _ c → do
      eT ← coreToType e
      e ← coreToExpr e
      c ← mapM transformAlt c
      let binder = case GHC.occInfo (GHC.idInfo b) of GHC.IAmDead → Nothing; _ → Just (prettyPrintValue b)
      tellReturn (Case e binder eT c)

    {- Ignore casts. -}
    GHC.Cast e _ → coreToExpr e

    {- Ignore ticks. -}
    GHC.Tick _ e → coreToExpr e

    {- Should be matched elsewhere. -}
    GHC.Type t → error t

    {- Not supported. -}
    GHC.Coercion c → error c

transformAlt ∷ GHC.Alt GHC.Var → CompilerM (CaseOption M.Type) M.Type
transformAlt (GHC.DEFAULT, [], expr) = do
  expr ← coreToExpr expr
  return (DefaultCase expr)
transformAlt (GHC.DataAlt dataCon, binds, expr) = do
  dataCon ← transformDataCon dataCon
  expr ← coreToExpr expr
  let modBinds = fmap ((\v → if uses v expr then Just v else Nothing) . prettyPrintValue) binds
  return (CaseOption dataCon modBinds expr)
transformAlt alt = throw (NotYetImplemented ("transformAlt: " `T.append` prettyPrintValue alt))

transformDataCon ∷ GHC.DataCon → CompilerM (DataCon M.Type) M.Type
transformDataCon dataCon = do
  repType ←
    case GHC.dataConType dataCon of
      GHC.TyConApp tC bs  → constructorReprType tC (GHC.dataConName dataCon) bs
      _                   → reprType [] dataCon
  return DataCon {
    conTag        = nameToTextSimple (GHC.dataConName dataCon),
    conRepType    = repType,
    conUnique     = GHC.getKey (GHC.getUnique dataCon)
  }
