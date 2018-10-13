module Juvix.Core.CompilerTypes where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Foundation

import qualified Juvix.Core.GHC           as GHC

data Literal
  = LUnit
  | LBool Bool
  | LKey  T.Text
  | LInt  Integer
  | LTez  Integer
  | LNat  Integer
  | LStr  T.Text
  | LPair Literal Literal

  deriving (Show, Eq)

data DataCon a
  = DataCon {
    conTag     ∷ T.Text,
    conRepType ∷ a,
    conUnique  ∷ Int
  }

  deriving (Show, Eq)

data CaseOption a
  = DefaultCase {
    defaultExpr       ∷ Expr a
  }
  | CaseOption {
    optionConstructor ∷ DataCon a,
    optionBinds       ∷ [Maybe T.Text],
    optionExpr        ∷ Expr a
  }

  deriving (Show, Eq)

data Expr a
  = BuiltIn T.Text
  | Lit Literal
  | Var T.Text
  | Let T.Text (Expr a) (Expr a)
  | App (Expr a) (Expr a)
  | Lam T.Text (Expr a)
  | Case (Expr a) (Maybe T.Text) a [CaseOption a]
  | BindIO    (Expr a) (Expr a)
  | SeqIO     (Expr a) (Expr a)
  | ReturnIO  (Expr a)
  | Ann (Expr a) a

  deriving (Show, Eq)

data CompileError =

  InvalidType |
  InvariantError |
  MainFunctionNotFound |
  VariableNotInScope T.Text StackRep |
  NotYetImplemented T.Text

data CompileLog a
  = FrontendToCore  GHC.CoreExpr GHC.Type
  | CoreToExpr      GHC.CoreExpr (Expr a)
  | SimplifiedExpr  (Expr a) (Expr a)
  | ExprToMichelson (Expr a) T.Text StackRep StackRep
  -- | Optimized       M.SomeExpr M.SomeExpr
  | Custom          T.Text

type StackRep = [StackObject]

data StackObject =

  BoundVariable T.Text |
  -- Const M.ConstUT |
  FuncResult

  deriving (Eq, Show)

newtype Env = Env {
  envExprs ∷ Map.Map T.Text GHC.CoreExpr
}

type CompilerM a b = ExceptT CompileError (RWS Env [CompileLog b] StackRep) a
