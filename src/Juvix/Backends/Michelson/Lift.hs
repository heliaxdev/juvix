module Juvix.Backends.Michelson.Lift where

import           Data.Typeable
import           Protolude                        hiding (Option (..),
                                                   notImplemented)

import           Juvix.Backends.Michelson.Typed
import qualified Juvix.Backends.Michelson.Untyped as U
import           Juvix.Utility

data TypecheckError
  = CannotUnify DynamicError
  | NotImplemented

{-  Lift an untyped Michelson instruction sequence, with an initial stack type, into a typed (GADT) Michelson instruction sequence and return stack type in a typesafe manner.
    The type of the instruction sequence is *not* known until runtime (hence the heavy existential / scoped type variable use).
    If typechecking fails, return useful information about why and where.
    See [https://stackoverflow.com/questions/38024458/type-juggling-with-existentials-at-runtime] for some background on this mishmash of GHC extensions, although we need to do something more intricate.
    This would be far nicer with dependent types.   -}

-- Do we need to also pass an expected return type? e.g. Either
-- Intermediary lift stage for type annotation of future instructions?
-- We do *know* the stack state from exprToMichelson, we could keep it through this stage.
-- https://github.com/tezos/tezos/blob/master/src/proto/alpha/script_ir_translator.ml
-- return a λ on type, i.e. ExprUT → SomeStack → ?
-- ok, this is the right idea, will need to refine
-- but type of what, e.g. LEFT 2; IF_LEFT {...} {...} - don't *know* type
-- the annotation route is correct I think

liftUntyped ∷ ∀ m . (MonadError TypecheckError m) ⇒ U.Expr → DynamicType → DynamicType → m (SomeExpr, DynamicType)
liftUntyped expr stk@(DynamicType (prx@(Proxy ∷ Proxy stkTy))) str@(DynamicType (Proxy ∷ Proxy storageTy)) = do

  let take1 ∷ (MonadError TypecheckError m) ⇒ m (DynamicType, DynamicType)
      take1 = undefined

      take2 ∷ (MonadError TypecheckError m) ⇒ m (DynamicType, DynamicType, DynamicType)
      take2 = undefined

      take3 ∷ (MonadError TypecheckError m) ⇒ m (DynamicType, DynamicType, DynamicType, DynamicType)
      take3 = undefined

      take4 ∷ (MonadError TypecheckError m) ⇒ m (DynamicType, DynamicType, DynamicType, DynamicType, DynamicType)
      take4 = undefined

      asUnion ∷ (MonadError TypecheckError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asUnion = undefined

      asProduct ∷ (MonadError TypecheckError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asProduct = undefined

      asArrow ∷ (MonadError TypecheckError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asArrow = undefined

  case expr of

    U.Drop → do
      (DynamicType (_ ∷ Proxy a), DynamicType (rest ∷ Proxy b)) ← take1
      return (SomeExpr (Drop ∷ Expr (Stack (a, b)) (Stack b)), DynamicType rest)

    U.Dup → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)) ← take1
      return (SomeExpr (Dup ∷ Expr (Stack (a, b)) (Stack (a, (a, b)))), DynamicType (Proxy ∷ Proxy (a, (a, b))))

    U.Swap → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      return (SomeExpr (Swap ∷ Expr (Stack (a, (b, c))) (Stack (b, (a, c)))), DynamicType (Proxy ∷ Proxy (b, (a, c))))

    _ -> notImplemented

cannotUnify ∷ DynamicError → (MonadError TypecheckError m) ⇒ m a
cannotUnify = throw . CannotUnify

notImplemented ∷ (MonadError TypecheckError m) ⇒ m a
notImplemented = throw NotImplemented

{-  Lift a Michelson type into a Haskell type existential.  -}

liftType ∷ U.Type → DynamicType
liftType U.UnitT          = toDynamicType (Proxy ∷ Proxy ())
liftType U.KeyT           = toDynamicType (Proxy ∷ Proxy Key)
liftType U.HashT          = toDynamicType (Proxy ∷ Proxy Hash)
liftType U.IntT           = toDynamicType (Proxy ∷ Proxy Integer)
liftType U.TezT           = toDynamicType (Proxy ∷ Proxy Tez)
liftType U.BoolT          = toDynamicType (Proxy ∷ Proxy Bool)
liftType U.StringT        = toDynamicType (Proxy ∷ Proxy Text)
liftType (U.EitherT a b)  =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy a), DynamicType (Proxy ∷ Proxy b)) → toDynamicType (Proxy ∷ Proxy (Union a b))
liftType (U.OptionT a) =
  case liftType a of
    DynamicType (Proxy ∷ Proxy a) → toDynamicType (Proxy ∷ Proxy (Option a))
liftType (U.ListT a) =
  case liftType a of
    DynamicType (Proxy ∷ Proxy a) → toDynamicType (Proxy ∷ Proxy (List a))
liftType (U.PairT a b) =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy x), DynamicType (Proxy ∷ Proxy y)) → toDynamicType (Proxy ∷ Proxy (Pair x y))
liftType (U.LamT a b) =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy x), DynamicType (Proxy ∷ Proxy y)) → toDynamicType (Proxy ∷ Proxy (x → y))

{- Starting stack type. -}

typeToStack ∷ U.Type → DynamicType
typeToStack ty =
  case liftType ty of
    DynamicType (Proxy ∷ Proxy t) → DynamicType (Proxy ∷ Proxy (t, ()))
