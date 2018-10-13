module Juvix.Transpiler.PrettyPrint where

import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Foundation
import qualified Prelude                        as P
import qualified System.Console.ANSI            as ANSI

import qualified Juvix.Backends.Michelson.Types as J (InterpretError (..),
                                                      SomeStack (..),
                                                      Stack (..), Type (..))
import           Juvix.Core
import qualified Juvix.Core.CompilerTypes       as J
import qualified Juvix.Core.GHC                 as GHC

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

ppCompileLog ∷ J.CompileLog J.Type → T.Text
ppCompileLog (J.Custom text)              = T.concat [withColor ANSI.Magenta "Custom ", text]
ppCompileLog (J.FrontendToCore core ty)      = T.concat [withColor ANSI.Magenta "Frontend ⇒ Core :\n\t=> ", prettyPrintValue core, "\n\t @ ", prettyPrintValue ty]
ppCompileLog (J.CoreToExpr core expr)     = T.concat [withColor ANSI.Magenta "Core ⇒ Expr : ", T.replace "  " " " ((T.replace "\n" "") (prettyPrintValue core)), withColor ANSI.Magenta "\n\t⇒ ", prettyPrintValue expr]
ppCompileLog (J.SimplifiedExpr a b)       = T.concat [withColor ANSI.Magenta "Expr ⇒ Expr : ", prettyPrintValue a, withColor ANSI.Magenta "\n\t⇒ ", prettyPrintValue b]
ppCompileLog (J.ExprToMichelson a b x y)  = T.concat [withColor ANSI.Magenta "Expr ⇒ Michelson : ", prettyPrintValue a, withColor ANSI.Magenta "\n\t⇒ ", prettyPrintValue b, "\n\t~ [", T.intercalate ", " (fmap prettyPrintValue x), "] ⇒ [", T.intercalate "," (fmap prettyPrintValue y), "]"]
--ppCompileLog (J.Optimized (J.SomeExpr a) (J.SomeExpr b))            = T.concat [withColor ANSI.Magenta "Michelson ⇒ Michelson : ", emit a, withColor ANSI.Magenta "\n\t⇒ ", emit b]

ppStackObject ∷ J.StackObject → T.Text
ppStackObject J.FuncResult        = withColor ANSI.Cyan "R"
--ppStackObject (J.Const c)         = T.concat [withColor ANSI.Yellow "Const ", prettyPrintValue c]
ppStackObject (J.BoundVariable v) = withColor ANSI.Green v

ppCompileError ∷ J.CompileError → T.Text
ppCompileError J.InvalidType              = withColor ANSI.Red "Invalid type"
ppCompileError J.InvariantError           = withColor ANSI.Red "Invariant error"
ppCompileError J.MainFunctionNotFound     = withColor ANSI.Red "Main function not found"
ppCompileError (J.VariableNotInScope v s) = T.concat [withColor ANSI.Red "Variable not in scope: ", withColor ANSI.Green v, "; stack [", T.intercalate ", " (fmap prettyPrintValue s), "]"]
ppCompileError (J.NotYetImplemented e)    = T.concat [withColor ANSI.Red "Not yet implemented: ", e]

ppJExpr ∷ J.Expr J.Type → T.Text
ppJExpr (J.BuiltIn e)     = T.concat [withColor ANSI.Cyan "BuiltIn {", prettyPrintValue e, withColor ANSI.Cyan "}"]
ppJExpr (J.Lit l)         = T.concat [withColor ANSI.Cyan "Lit {", prettyPrintValue l, "}"]
ppJExpr (J.Var v)         = withColor ANSI.Green v
ppJExpr (J.Let x y z)     = T.concat ["let ", withColor ANSI.Green x, " = ", prettyPrintValue y, " in ", prettyPrintValue z]
ppJExpr (J.App x y)       = T.concat ["(", prettyPrintValue x, ") (", prettyPrintValue y, ")"]
ppJExpr (J.Lam v e)       = T.concat [withColor ANSI.Yellow "\\", withColor ANSI.Green v, withColor ANSI.Yellow " → ", prettyPrintValue e]
ppJExpr (J.Case e b t o)  = T.concat ["case ", prettyPrintValue e, " as ", case b of Just b → withColor ANSI.Green b; Nothing → "_", " @ ", prettyPrintValue t, " of ", T.intercalate ", " (fmap prettyPrintValue o)]
ppJExpr (J.BindIO x y)    = T.concat [prettyPrintValue x, withColor ANSI.Yellow " >>= ", prettyPrintValue y]
ppJExpr (J.SeqIO x y)     = T.concat [prettyPrintValue x, withColor ANSI.Yellow " >> ", prettyPrintValue y]
ppJExpr (J.ReturnIO x)    = T.concat [withColor ANSI.Yellow "return ", prettyPrintValue x]
ppJExpr (J.Ann x t)       = T.concat ["(", prettyPrintValue x, withColor ANSI.Yellow " ∷ ", prettyPrintValue t, ")"]

ppLiteral ∷ J.Literal → T.Text
ppLiteral = T.pack . P.show

ppCaseOption ∷ J.CaseOption J.Type → T.Text
ppCaseOption (J.DefaultCase e)    = T.concat ["DEFAULT → ", prettyPrintValue e]
ppCaseOption (J.CaseOption d b e) = T.concat [prettyPrintValue d, " ~ ", T.intercalate ", " (fmap prettyPrintValue b), " → ", prettyPrintValue e]

ppJDataCon ∷ J.DataCon J.Type → T.Text
ppJDataCon (J.DataCon tag _ _) = tag

ppTyThing ∷ GHC.TyThing → T.Text
ppTyThing (GHC.AnId v)     = T.concat ["VarT {", prettyPrintValue v, "}"]
ppTyThing (GHC.AConLike c) = T.concat ["ConLikeT {", prettyPrintValue c, "}"]
ppTyThing (GHC.ATyCon c)   = T.concat ["TyConT {", prettyPrintValue c, "}"]
ppTyThing _                = T.concat ["TyThing"]

ppConLike ∷ GHC.ConLike → T.Text
ppConLike _ = T.concat ["ConLike"]

ppMod ∷ GHC.CoreModule → T.Text
ppMod (GHC.CoreModule _ _ bs _) = T.unlines [
  "TypeEnv: ",
  --T.intercalate "\n" $ fmap (\(x, y) → T.concat [prettyPrintValue x, " ⇒ ", prettyPrintValue y]) $ GHC.ufmToList nameEnv,
  "Bindings: ",
  T.intercalate "\n" $ fmap prettyPrintValue bs
  ]

ppUnique ∷ GHC.Unique → T.Text
ppUnique _ = "Unique"

ppExpr ∷ GHC.CoreExpr → T.Text
ppExpr = ppOutputable
--ppExpr = ppExpr'

{-
ppExpr' ∷ GHC.CoreExpr → T.Text
ppExpr' (GHC.Var v)          = T.concat ["VarE (", prettyPrintValue v, ")"]
ppExpr' (GHC.Lit l)          = T.concat ["LitE (", prettyPrintValue l, ")"]
ppExpr' (GHC.App x y)        = T.concat ["AppE (", ppExpr x, ") (", ppExpr y, ")"]
ppExpr' (GHC.Lam x y)        = T.concat ["LamE (", prettyPrintValue x, ") (", ppExpr y, ")"]
ppExpr' (GHC.Let b e)        = T.concat ["LetE (", prettyPrintValue b, ") (", ppExpr e, ")"]
ppExpr' (GHC.Case e v t xs)  = T.concat ["CaseE (", ppExpr e, ") (", prettyPrintValue v, ") (", prettyPrintValue t, ") [", T.intercalate ", " $ fmap prettyPrintValue xs, "]"]
ppExpr' (GHC.Tick _ e)       = T.concat ["TickE (", ppExpr e, ")"]
ppExpr' (GHC.Type t)         = T.concat ["TypeE (", prettyPrintValue t, ")"]
ppExpr' (GHC.Coercion _)     = T.concat ["CoercionE ()"]
ppExpr' _                    = T.concat ["Expr..."]
-}

ppAlt ∷ GHC.Alt GHC.CoreBndr → T.Text
ppAlt (x, v, e) = T.concat ["{", prettyPrintValue x, " - ", T.intercalate ", " $ fmap prettyPrintValue v, " - ", prettyPrintValue e, "}"]

ppAltCon ∷ GHC.AltCon → T.Text
ppAltCon (GHC.DataAlt d) = T.concat ["DataAltCon (", prettyPrintValue d, ")"]
ppAltCon (GHC.LitAlt l)  = T.concat ["LitAltCon (", prettyPrintValue l, ")"]
ppAltCon GHC.DEFAULT     = T.concat ["AltConDefault"]

ppDataCon ∷ GHC.DataCon → T.Text
ppDataCon = prettyPrintValue . GHC.dataConName

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
ppLit (GHC.LitInteger i t)    = T.concat ["IntegerL (", T.pack $ P.show i, ") @ (", prettyPrintValue t, ")"]

ppVar ∷ GHC.Var → T.Text
ppVar = nameToText . GHC.varName

ppType ∷ GHC.Type → T.Text
--ppType = ppOutputable
ppType = ppType'

ppType' ∷ GHC.Type → T.Text
ppType' (GHC.TyVarTy v) = prettyPrintValue v
ppType' (GHC.AppTy x y) = T.concat ["AppT (", ppType x, ") (", ppType y, ")"]
ppType' (GHC.TyConApp c t) = T.concat ["ConT (", prettyPrintValue c, ") [", T.intercalate "," $ fmap prettyPrintValue t, "]"]
ppType' (GHC.CastTy ty _) = prettyPrintValue ty
ppType' (GHC.LitTy _)    = "LitTy"
ppType' (GHC.CoercionTy _) = "CoercionTy"
ppType' (GHC.ForAllTy _ t) = T.concat ["ForAllT (", "<v>", ") {", prettyPrintValue t, "}"]
ppType' (GHC.FunTy x y) = T.concat ["FunT (", prettyPrintValue x, ") (", prettyPrintValue y, ")"]

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
ppBind (GHC.NonRec b e) = T.concat ["NonRecB (", prettyPrintValue b, ") (", prettyPrintValue e, ")"]
ppBind (GHC.Rec xs)     = T.concat ["RecB [", T.concat $ fmap (\(x, y) → T.concat ["(", prettyPrintValue x, ") (", prettyPrintValue y, ")"]) xs, "]"]

nameToText ∷ GHC.Name → T.Text
nameToText name =
  let occName = GHC.nameOccName name in
  T.concat [T.decodeUtf8 $ GHC.fs_bs $ GHC.occNameFS occName, "_", T.pack $ P.show $ GHC.getUnique occName]

instance PrettyPrint GHC.Var where prettyPrintValue = ppVar
instance PrettyPrint GHC.Coercion where prettyPrintValue _ = "Coercion"
instance PrettyPrint GHC.CoreExpr where prettyPrintValue = ppExpr
instance PrettyPrint GHC.Type where prettyPrintValue = ppType
instance PrettyPrint GHC.TyCon where prettyPrintValue = ppTycon
instance PrettyPrint GHC.Literal where prettyPrintValue = ppLit
instance PrettyPrint GHC.CoreBind where prettyPrintValue = ppBind
instance PrettyPrint GHC.CoreModule where prettyPrintValue = ppMod
instance PrettyPrint (GHC.Alt GHC.CoreBndr) where prettyPrintValue = ppAlt
instance PrettyPrint GHC.AltCon where prettyPrintValue = ppAltCon
instance PrettyPrint GHC.TyThing where prettyPrintValue = ppTyThing
instance PrettyPrint GHC.ConLike where prettyPrintValue = ppConLike
instance PrettyPrint GHC.Unique where prettyPrintValue = ppUnique
instance PrettyPrint GHC.Name where prettyPrintValue = nameToText
instance PrettyPrint GHC.DataCon where prettyPrintValue = ppDataCon
instance PrettyPrint (J.Expr J.Type) where prettyPrintValue = ppJExpr
instance PrettyPrint J.Literal where prettyPrintValue = ppLiteral
instance PrettyPrint (J.CaseOption J.Type) where prettyPrintValue = ppCaseOption
instance PrettyPrint (J.DataCon J.Type) where prettyPrintValue = ppJDataCon
instance PrettyPrint (J.CompileLog J.Type) where prettyPrintValue = ppCompileLog
instance PrettyPrint J.CompileError where prettyPrintValue = ppCompileError
instance PrettyPrint J.InterpretError where prettyPrintValue _ = ""
--instance PrettyPrint J.ExprUT where prettyPrintValue = emitUT
instance PrettyPrint J.StackObject where prettyPrintValue = ppStackObject
--instance PrettyPrint J.Type where prettyPrintValue = T.pack . P.show
instance PrettyPrint GHC.TyBinder where prettyPrintValue = ppOutputable
instance PrettyPrint GHC.FieldLabelEnv where prettyPrintValue = ppOutputable
--instance PrettyPrint J.ConstUT where prettyPrintValue = emitUT . J.ConstUT

instance (PrettyPrint k, PrettyPrint v, Typeable k, Typeable v) ⇒ PrettyPrint (Map.Map k v) where
  prettyPrintValue = (\x → T.concat ["{ ", x, " }"]) . T.intercalate ", " . fmap (\(x, y) → T.concat [prettyPrintValue x, " ⇒ ", prettyPrintValue y]) . Map.toList

-- todo this file
