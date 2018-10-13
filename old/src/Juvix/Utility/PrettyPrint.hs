module Juvix.Utility.PrettyPrint (
  PrettyPrint,
  pprint,
  nameToTextSimple,
  typeName
) where

import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Foundation
import qualified Prelude                        as P
import qualified System.Console.ANSI            as ANSI

import           Juvix.Backends.Michelson.Emit  (emit, emitUT)
import qualified Juvix.Backends.Michelson.Types as J (ConstUT, ExprUT (..),
                                                      InterpretError (..),
                                                      SomeExpr (..),
                                                      SomeStack (..),
                                                      Stack (..), Type (..))
import qualified Juvix.Core.CompilerTypes       as J
import qualified Juvix.Core.GHC                 as GHC
import           Juvix.Utility.Types

import qualified GHC.Paths                      as Paths
import           System.IO.Unsafe

{-  This should be changed, although the fault lies equally with GHC's arcane "Outputable" API.   -}

{-# NOINLINE dynFlags #-}
dynFlags ∷ GHC.DynFlags
dynFlags = unsafePerformIO (GHC.runGhc (Just Paths.libdir) GHC.getDynFlags)

withSGR ∷ [ANSI.SGR] → T.Text → T.Text
withSGR sgr str = T.concat [T.pack (ANSI.setSGRCode sgr), str, T.pack (ANSI.setSGRCode [ANSI.Reset])]

withColor ∷ ANSI.Color → T.Text → T.Text
withColor color = withSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull color]

nameToTextSimple ∷ GHC.Name → T.Text
nameToTextSimple = T.decodeUtf8 . GHC.fs_bs . GHC.occNameFS . GHC.nameOccName

typeName ∷ GHC.Type → GHC.Name
typeName (GHC.TyVarTy v)    = GHC.varName v
typeName (GHC.AppTy _ y)    = typeName y
typeName (GHC.TyConApp c _) = GHC.tyConName c
typeName (GHC.CastTy x _)   = typeName x
typeName (GHC.ForAllTy _ t) = typeName t
typeName _                  = undefined

ppOutputable ∷ (GHC.Outputable a) ⇒ a → T.Text
ppOutputable = T.pack . GHC.showSDoc dynFlags . GHC.ppr

ppCompileLog ∷ J.CompileLog → T.Text
ppCompileLog (J.Custom text)              = T.concat [withColor ANSI.Magenta "Custom ", text]
ppCompileLog (J.FrontendToCore core ty)      = T.concat [withColor ANSI.Magenta "Frontend ⇒ Core :\n\t=> ", pprint core, "\n\t @ ", pprint ty]
ppCompileLog (J.CoreToExpr core expr)     = T.concat [withColor ANSI.Magenta "Core ⇒ Expr : ", T.replace "  " " " ((T.replace "\n" "") (pprint core)), withColor ANSI.Magenta "\n\t⇒ ", pprint expr]
ppCompileLog (J.SimplifiedExpr a b)       = T.concat [withColor ANSI.Magenta "Expr ⇒ Expr : ", pprint a, withColor ANSI.Magenta "\n\t⇒ ", pprint b]
ppCompileLog (J.ExprToMichelson a b x y)  = T.concat [withColor ANSI.Magenta "Expr ⇒ Michelson : ", pprint a, withColor ANSI.Magenta "\n\t⇒ ", pprint b, "\n\t~ [", T.intercalate ", " (fmap pprint x), "] ⇒ [", T.intercalate "," (fmap pprint y), "]"]
ppCompileLog (J.Optimized (J.SomeExpr a) (J.SomeExpr b))            = T.concat [withColor ANSI.Magenta "Michelson ⇒ Michelson : ", emit a, withColor ANSI.Magenta "\n\t⇒ ", emit b]

ppStackObject ∷ J.StackObject → T.Text
ppStackObject J.FuncResult        = withColor ANSI.Cyan "R"
ppStackObject (J.Const c)         = T.concat [withColor ANSI.Yellow "Const ", pprint c]
ppStackObject (J.BoundVariable v) = withColor ANSI.Green v

ppCompileError ∷ J.CompileError → T.Text
ppCompileError J.InvalidType              = withColor ANSI.Red "Invalid type"
ppCompileError J.InvariantError           = withColor ANSI.Red "Invariant error"
ppCompileError J.MainFunctionNotFound     = withColor ANSI.Red "Main function not found"
ppCompileError (J.VariableNotInScope v s) = T.concat [withColor ANSI.Red "Variable not in scope: ", withColor ANSI.Green v, "; stack [", T.intercalate ", " (fmap pprint s), "]"]
ppCompileError (J.NotYetImplemented e)    = T.concat [withColor ANSI.Red "Not yet implemented: ", e]

ppJExpr ∷ J.Expr → T.Text
ppJExpr (J.BuiltIn e)     = T.concat [withColor ANSI.Cyan "BuiltIn {", pprint e, withColor ANSI.Cyan "}"]
ppJExpr (J.Lit l)         = T.concat [withColor ANSI.Cyan "Lit {", pprint l, "}"]
ppJExpr (J.Var v)         = withColor ANSI.Green v
ppJExpr (J.Let x y z)     = T.concat ["let ", withColor ANSI.Green x, " = ", pprint y, " in ", pprint z]
ppJExpr (J.App x y)       = T.concat ["(", pprint x, ") (", pprint y, ")"]
ppJExpr (J.Lam v e)       = T.concat [withColor ANSI.Yellow "\\", withColor ANSI.Green v, withColor ANSI.Yellow " → ", pprint e]
ppJExpr (J.Case e b t o)  = T.concat ["case ", pprint e, " as ", case b of Just b → withColor ANSI.Green b; Nothing → "_", " @ ", pprint t, " of ", T.intercalate ", " (fmap pprint o)]
ppJExpr (J.BindIO x y)    = T.concat [pprint x, withColor ANSI.Yellow " >>= ", pprint y]
ppJExpr (J.SeqIO x y)     = T.concat [pprint x, withColor ANSI.Yellow " >> ", pprint y]
ppJExpr (J.ReturnIO x)    = T.concat [withColor ANSI.Yellow "return ", pprint x]
ppJExpr (J.Ann x t)       = T.concat ["(", pprint x, withColor ANSI.Yellow " ∷ ", pprint t, ")"]

ppLiteral ∷ J.Literal → T.Text
ppLiteral = T.pack . P.show

ppCaseOption ∷ J.CaseOption → T.Text
ppCaseOption (J.DefaultCase e)    = T.concat ["DEFAULT → ", pprint e]
ppCaseOption (J.CaseOption d b e) = T.concat [pprint d, " ~ ", T.intercalate ", " (fmap pprint b), " → ", pprint e]

ppJDataCon ∷ J.DataCon → T.Text
ppJDataCon (J.DataCon tag _ _) = tag

ppTyThing ∷ GHC.TyThing → T.Text
ppTyThing (GHC.AnId v)     = T.concat ["VarT {", pprint v, "}"]
ppTyThing (GHC.AConLike c) = T.concat ["ConLikeT {", pprint c, "}"]
ppTyThing (GHC.ATyCon c)   = T.concat ["TyConT {", pprint c, "}"]
ppTyThing _                = T.concat ["TyThing"]

ppConLike ∷ GHC.ConLike → T.Text
ppConLike _ = T.concat ["ConLike"]

ppMod ∷ GHC.CoreModule → T.Text
ppMod (GHC.CoreModule _ nameEnv bs _) = T.unlines [
  "TypeEnv: ",
  --T.intercalate "\n" $ fmap (\(x, y) → T.concat [pprint x, " ⇒ ", pprint y]) $ GHC.ufmToList nameEnv,
  "Bindings: ",
  T.intercalate "\n" $ fmap pprint bs
  ]

ppUnique ∷ GHC.Unique → T.Text
ppUnique _ = "Unique"

ppExpr ∷ GHC.CoreExpr → T.Text
ppExpr = ppOutputable
--ppExpr = ppExpr'

{-
ppExpr' ∷ GHC.CoreExpr → T.Text
ppExpr' (GHC.Var v)          = T.concat ["VarE (", pprint v, ")"]
ppExpr' (GHC.Lit l)          = T.concat ["LitE (", pprint l, ")"]
ppExpr' (GHC.App x y)        = T.concat ["AppE (", ppExpr x, ") (", ppExpr y, ")"]
ppExpr' (GHC.Lam x y)        = T.concat ["LamE (", pprint x, ") (", ppExpr y, ")"]
ppExpr' (GHC.Let b e)        = T.concat ["LetE (", pprint b, ") (", ppExpr e, ")"]
ppExpr' (GHC.Case e v t xs)  = T.concat ["CaseE (", ppExpr e, ") (", pprint v, ") (", pprint t, ") [", T.intercalate ", " $ fmap pprint xs, "]"]
ppExpr' (GHC.Tick _ e)       = T.concat ["TickE (", ppExpr e, ")"]
ppExpr' (GHC.Type t)         = T.concat ["TypeE (", pprint t, ")"]
ppExpr' (GHC.Coercion _)     = T.concat ["CoercionE ()"]
ppExpr' _                    = T.concat ["Expr..."]
-}

ppAlt ∷ GHC.Alt GHC.CoreBndr → T.Text
ppAlt (x, v, e) = T.concat ["{", pprint x, " - ", T.intercalate ", " $ fmap pprint v, " - ", pprint e, "}"]

ppAltCon ∷ GHC.AltCon → T.Text
ppAltCon (GHC.DataAlt d) = T.concat ["DataAltCon (", pprint d, ")"]
ppAltCon (GHC.LitAlt l)  = T.concat ["LitAltCon (", pprint l, ")"]
ppAltCon GHC.DEFAULT     = T.concat ["AltConDefault"]

ppDataCon ∷ GHC.DataCon → T.Text
ppDataCon = pprint . GHC.dataConName

ppLit ∷ GHC.Literal → T.Text
ppLit (GHC.MachChar c)        = T.concat ["CharL (", T.pack [c], ")"]
ppLit (GHC.MachStr b)         = T.concat ["StrL (", T.decodeUtf8 b, ")"]
ppLit GHC.MachNullAddr        = T.concat ["NullL"]
ppLit (GHC.MachInt i)         = T.concat ["IntL (", T.pack $ P.show i, ")"]
ppLit (GHC.MachInt64 i)       = T.concat ["Int64L (", T.pack $ P.show i, ")"]
ppLit (GHC.MachWord i)        = T.concat ["WordL (", T.pack $ P.show i, ")"]
ppLit (GHC.MachWord64 i)      = T.concat ["Word64L (", T.pack $ P.show i, ")"]
ppLit (GHC.MachFloat r)       = T.concat ["FloatL (", T.pack $ P.show r, ")"]
ppLit (GHC.MachDouble r)      = T.concat ["DoubleL (", T.pack $ P.show r, ")"]
ppLit (GHC.MachLabel f mi _)  = T.concat ["LabelL (", T.decodeUtf8 $ GHC.fs_bs f, ") (", T.pack $ P.show mi, ") ()"]
ppLit (GHC.LitInteger i t)    = T.concat ["IntegerL (", T.pack $ P.show i, ") @ (", pprint t, ")"]

ppVar ∷ GHC.Var → T.Text
ppVar = nameToText . GHC.varName

ppType ∷ GHC.Type → T.Text
--ppType = ppOutputable
ppType = ppType'

ppType' ∷ GHC.Type → T.Text
ppType' (GHC.TyVarTy v) = pprint v
ppType' (GHC.AppTy x y) = T.concat ["AppT (", ppType x, ") (", ppType y, ")"]
ppType' (GHC.TyConApp c t) = T.concat ["ConT (", pprint c, ") [", T.intercalate "," $ fmap pprint t, "]"]
ppType' (GHC.CastTy ty _) = pprint ty
ppType' (GHC.LitTy _)    = "LitTy"
ppType' (GHC.CoercionTy _) = "CoercionTy"
--ppType' (GHC.ForAllTy v t) = T.concat ["ForAllT (", pprint v, ") {", pprint t, "}"]

ppTycon ∷ GHC.TyCon → T.Text
ppTycon = ppOutputable

{-
ppTycon c =
  let n = nameToText $ GHC.tyConName c
  in if
    | GHC.isFunTyCon c → T.concat ["FunT (", n, ")"]
    | GHC.isAlgTyCon c → T.concat ["AlgT (", n, ")"]
    | GHC.isTypeSynonymTyCon c → T.concat ["SynT (", n, ")"]
    | GHC.isFamilyTyCon c → T.concat ["FamT (", n, ")"]
    | GHC.isPrimTyCon c → T.concat ["PrimT (", n, ")"]
    | GHC.isPromotedDataCon c → T.concat ["PromT (", n, ")"]
    | GHC.isTcTyCon c → T.concat ["TcTyT (", n, ")"]
-}

ppBind ∷ GHC.CoreBind → T.Text
ppBind (GHC.NonRec b e) = T.concat ["NonRecB (", pprint b, ") (", pprint e, ")"]
ppBind (GHC.Rec xs)     = T.concat ["RecB [", T.concat $ fmap (\(x, y) → T.concat ["(", pprint x, ") (", pprint y, ")"]) xs, "]"]

nameToText ∷ GHC.Name → T.Text
nameToText name =
  let occName = GHC.nameOccName name in
  T.concat [T.decodeUtf8 $ GHC.fs_bs $ GHC.occNameFS occName, "_", T.pack $ P.show $ GHC.getUnique occName]

ppStack ∷ J.SomeStack → T.Text
ppStack (J.SomeStack stk) =
  case stk of
    J.Empty    → "|"
    J.Item x s → T.concat ["x", " : ", pprint (J.SomeStack s)]

instance PrettyPrint GHC.Var where pprint = ppVar
instance PrettyPrint GHC.Coercion where pprint _ = "Coercion"
instance PrettyPrint GHC.CoreExpr where pprint = ppExpr
instance PrettyPrint GHC.Type where pprint = ppType
instance PrettyPrint GHC.TyCon where pprint = ppTycon
instance PrettyPrint GHC.Literal where pprint = ppLit
instance PrettyPrint GHC.CoreBind where pprint = ppBind
instance PrettyPrint GHC.CoreModule where pprint = ppMod
instance PrettyPrint (GHC.Alt GHC.CoreBndr) where pprint = ppAlt
instance PrettyPrint GHC.AltCon where pprint = ppAltCon
instance PrettyPrint GHC.TyThing where pprint = ppTyThing
instance PrettyPrint GHC.ConLike where pprint = ppConLike
instance PrettyPrint GHC.Unique where pprint = ppUnique
instance PrettyPrint GHC.Name where pprint = nameToText
instance PrettyPrint GHC.DataCon where pprint = ppDataCon
instance PrettyPrint J.Expr where pprint = ppJExpr
instance PrettyPrint J.Literal where pprint = ppLiteral
instance PrettyPrint J.CaseOption where pprint = ppCaseOption
instance PrettyPrint J.DataCon where pprint = ppJDataCon
instance PrettyPrint J.CompileLog where pprint = ppCompileLog
instance PrettyPrint J.CompileError where pprint = ppCompileError
instance PrettyPrint J.InterpretError where pprint _ = ""
instance PrettyPrint J.ExprUT where pprint = emitUT
instance PrettyPrint J.StackObject where pprint = ppStackObject
instance PrettyPrint J.Type where pprint = T.pack . P.show
instance PrettyPrint GHC.TyBinder where pprint = ppOutputable
instance PrettyPrint GHC.FieldLabelEnv where pprint = ppOutputable
instance PrettyPrint J.ConstUT where pprint = emitUT . J.ConstUT
instance PrettyPrint J.SomeStack where pprint = ppStack

instance PrettyPrint T.Text where pprint = id

instance (PrettyPrint k, PrettyPrint v) ⇒ PrettyPrint (Map.Map k v) where
  pprint = (\x → T.concat ["{ ", x, " }"]) . T.intercalate ", " . fmap (\(x, y) → T.concat [pprint x, " ⇒ ", pprint y]) . Map.toList

instance (PrettyPrint a) ⇒ PrettyPrint [a] where
  pprint = T.intercalate ", " . fmap pprint

instance (PrettyPrint a) ⇒ PrettyPrint (Maybe a) where
  pprint (Just x) = T.concat ["Just ", pprint x]
  pprint Nothing  = "Nothing"
