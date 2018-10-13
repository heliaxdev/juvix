module Juvix.Transpiler.TypeToType where

import qualified Data.Text                    as T
import           Foundation
import qualified Prelude                      as P

import qualified Juvix.Backends.Michelson     as M
import           Juvix.Core
import           Juvix.Core.CompilerTypes
import qualified Juvix.Core.GHC               as GHC
import           Juvix.Transpiler.PrettyPrint
import           Juvix.Transpiler.Utility

typeToType ∷ GHC.Type → CompilerM M.Type M.Type
typeToType ty = do
  case ty of
    GHC.TyVarTy v → typeToType (GHC.varType v)
    GHC.TyConApp c [x, y] | GHC.isAlgTyCon c && nameToTextSimple (GHC.tyConName c) == "(,)" → do
      x ← typeToType x
      y ← typeToType y
      return (M.PairT x y)
    GHC.TyConApp c [x] | nameToTextSimple (GHC.tyConName c) == "IO" → do
      typeToType x
    GHC.TyConApp c bs | GHC.isAlgTyCon c ->
      case nameToTextSimple (GHC.tyConName c) of
        "()"      → return M.UnitT
        "String"  → return M.StringT
        "Int"     → return M.IntT
        "Tez"     → return M.TezT
        "Key"     → return M.KeyT
        "Hash"    → return M.HashT
        _         → do
          let constructors = GHC.visibleDataCons (GHC.algTyConRhs c)
          reprType ← algReprType constructors bs
          return reprType
    GHC.FunTy x y -> do
      x <- typeToType x
      y <- typeToType y
      return (M.LamT x y)
    GHC.ForAllTy (GHC.TvBndr x _) y → do
      x ← typeToType (GHC.TyVarTy x)
      y ← typeToType y
      return (M.LamT x y)
    GHC.TyConApp c tys | GHC.isTypeSynonymTyCon c ->
      case GHC.synTyConRhs_maybe c of
        Just (GHC.ForAllTy (GHC.TvBndr x _) y) ->
          case tys of
            [arg] -> typeToType (substTy x arg y)
            _    -> throw (NotYetImplemented (T.concat ["typeToType (syn): ", prettyPrintValue ty]))
        Just t | length tys == 0 -> typeToType t
        Just t -> throw (NotYetImplemented (T.concat ["typeToType (syn2): ", prettyPrintValue t]))
        _ → throw (NotYetImplemented (T.concat ["typeToType (syn): ", prettyPrintValue ty]))
    GHC.TyConApp _ [GHC.TyVarTy v] → typeToType (GHC.varType v) -- Double-check correctness here.
    ty → throw (NotYetImplemented (T.concat ["typeToType (unmatched): ", prettyPrintValue ty, " expanded: ", prettyPrintValue (GHC.coreView ty)]))

constructorReprType ∷ GHC.TyCon → GHC.Name → [GHC.Type] → CompilerM M.Type M.Type
constructorReprType tyCon name binds = do
  let constructors = GHC.visibleDataCons (GHC.algTyConRhs tyCon)
  case filter ((==) (nameToTextSimple name) . nameToTextSimple . GHC.dataConName) constructors of
    [dataCon] → reprType binds dataCon `catch` (\_ → throw (NotYetImplemented ("constructorReprType: " `T.append` prettyPrintValue tyCon)))
    _         → throw (NotYetImplemented ("constructorReprType (data constructor not found): " `T.append` prettyPrintValue name))

constructorPack ∷ GHC.TyCon → GHC.Name → [GHC.Type] → CompilerM (Expr M.Type) M.Type
constructorPack tyCon name binds = do
  let constructors = GHC.visibleDataCons (GHC.algTyConRhs tyCon)
  case filter ((==) (nameToTextSimple name) . nameToTextSimple . GHC.dataConName . snd) (P.zip [0..] constructors) of
    [(index, dataCon)]    → do
      let either = packEither index (P.length constructors)
      reprType ← reprType binds dataCon `catch` (\_ → throw (NotYetImplemented (T.concat ["constructorPack: ", prettyPrintValue tyCon, ", ", prettyPrintValue binds])))
      case reprType of
        M.UnitT → return (BuiltIn (T.pack (P.show (M.SeqUT (M.ConstUT M.UnitUT) either))))
        _ → return (BuiltIn (T.pack (P.show either)))
    _                     → throw (NotYetImplemented ("constructorPack (data constructor not found): " `T.append` prettyPrintValue name))

seqRepeat ∷ M.ExprUT → Int → M.ExprUT
seqRepeat _    0 = M.NopUT
seqRepeat expr 1 = expr
seqRepeat expr n = M.SeqUT (seqRepeat expr (n - 1)) expr

packEither ∷ Int → Int → M.ExprUT
packEither position count =
  let rights  = seqRepeat M.RightUT position
      lefts   = seqRepeat M.LeftUT (min 1 (count - position - 1))
  in M.SeqUT lefts rights

algReprType ∷ [GHC.DataCon] → [GHC.Type] → CompilerM M.Type M.Type
algReprType []  _     = return M.UnitT
algReprType tys binds = nestedEither =<< P.mapM (reprType binds) tys

reprType ∷ [GHC.Type] → GHC.DataCon → CompilerM M.Type M.Type
reprType binds dataCon = do
  let repType   = GHC.dataConRepType dataCon
  unarrowed ← unarrow binds repType
  case P.take (P.length unarrowed - 1) unarrowed of
    [] → return M.UnitT
    l  → do
            l ← P.mapM typeToType l `catch` (\_ → throw (NotYetImplemented ("reprType: " `T.append` prettyPrintValue dataCon `T.append` " ~ " `T.append` prettyPrintValue repType `T.append` " unarrowed: " `T.append` prettyPrintValue unarrowed `T.append` " @ " `T.append` prettyPrintValue l)))
            nestedPair l

nestedPair ∷ [M.Type] → CompilerM M.Type M.Type
nestedPair = nested M.PairT

nestedEither ∷ [M.Type] → CompilerM M.Type M.Type
nestedEither = nested M.EitherT

nested ∷ (M.Type → M.Type → M.Type) → [M.Type] → CompilerM M.Type M.Type
nested func =
  \case
    []      → throw (NotYetImplemented "nested: empty type list")
    [ty]    → return ty
    (t:ts)  → func t |<< nested func ts

unarrow ∷ [GHC.Type] → GHC.Type → CompilerM [GHC.Type] M.Type
unarrow binds ty =
  case ty of
    GHC.FunTy x y -> do
      (<>) [x] |<< unarrow binds y
    GHC.ForAllTy (GHC.TvBndr x _) y → do
      case binds of
        b:bs  → do
          unarrow bs (typeApply x b y)
        [] → return [ty]
    x                           → return [x]
