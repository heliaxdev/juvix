module Juvix.Backends.Michelson.Lift where

import qualified Data.Text                      as T
import           Data.Typeable
import           Foundation                     hiding (Either (..))
import qualified Foundation                     as F (Either (..))

import           Juvix.Backends.Michelson.Types
import           Juvix.Core
import           Juvix.Core.CompilerTypes       (CompileError (..))

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

liftUntyped ∷ ∀ m . (MonadError CompileError m) ⇒ ExprUT → DynamicType → DynamicType → m (SomeExpr, DynamicType)
liftUntyped expr stk@(DynamicType (prx@(Proxy ∷ Proxy stkTy))) str@(DynamicType (Proxy ∷ Proxy storageTy)) = do

  let cannotUnify ∷ DynamicError → (MonadError CompileError m) ⇒ m a
      cannotUnify e = throw (NotYetImplemented (T.concat ["liftUntyped - cannot unify ", prettyPrintValue expr, " at stack ", prettyPrintProxy prx, ": ", prettyPrintValue e]))

      notImplemented ∷ (MonadError CompileError m) ⇒ m a
      notImplemented = throw (NotYetImplemented (T.concat ["liftUntyped - not implemented: expr ", prettyPrintValue expr, " at stack ", prettyPrintProxy prx]))

      take1 ∷ (MonadError CompileError m) ⇒ m (DynamicType, DynamicType)
      take1 = case unProduct prx of
                F.Left e  → cannotUnify e
                F.Right r → return r

      take1As ∷ (MonadError CompileError m) ⇒ (DynamicType → m b) → m (b, DynamicType)
      take1As func = do
        (head, rest) ← take1
        maybe ← func head
        return (maybe, rest)

      asUnion ∷ (MonadError CompileError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asUnion (DynamicType prx) =
        case unSum prx of
          F.Left e  → cannotUnify e
          F.Right r → return r

      asProduct ∷ (MonadError CompileError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asProduct (DynamicType prx) =
        case unProduct prx of
          F.Left e  → cannotUnify e
          F.Right r → return r

      asArrow ∷ (MonadError CompileError m) ⇒ DynamicType → m (DynamicType, DynamicType)
      asArrow (DynamicType prx) =
        case unArrow prx of
          F.Left e  → cannotUnify e
          F.Right r → return r

      take1Pair ∷ (MonadError CompileError m) ⇒ m ((DynamicType, DynamicType), DynamicType)
      take1Pair = take1As asProduct

      take1Union ∷ (MonadError CompileError m) ⇒ m ((DynamicType, DynamicType), DynamicType)
      take1Union = take1As asUnion

      take2 ∷ (MonadError CompileError m) ⇒ m (DynamicType, DynamicType, DynamicType)
      take2 = do
        (x, DynamicType r) ← take1
        case unProduct r of
          F.Left e       → cannotUnify e
          F.Right (y, z) → return (x, y, z)

      take3 ∷ (MonadError CompileError m) ⇒ m (DynamicType, DynamicType, DynamicType, DynamicType)
      take3 = do
        (x, y, DynamicType r) ← take2
        case unProduct r of
          F.Left e       → cannotUnify e
          F.Right (a, b) → return (x, y, a, b)

      take4 ∷ (MonadError CompileError m) ⇒ m (DynamicType, DynamicType, DynamicType, DynamicType, DynamicType)
      take4 = do
        (x, y, z, DynamicType r) ← take3
        case unProduct r of
          F.Left e       → cannotUnify e
          F.Right (a, b) → return (x, y, z, a, b)

  case expr of

    DropUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (rest ∷ Proxy b)) ← take1
      return (SomeExpr (Drop ∷ Expr (Stack (a, b)) (Stack b)), DynamicType rest)

    DupUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)) ← take1
      return (SomeExpr (Dup ∷ Expr (Stack (a, b)) (Stack (a, (a, b)))), DynamicType (Proxy ∷ Proxy (a, (a, b))))

    SwapUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      return (SomeExpr (Swap ∷ Expr (Stack (a, (b, c))) (Stack (b, (a, c)))), DynamicType (Proxy ∷ Proxy (b, (a, c))))

    ConstUT c ->
      case c of
        UnitUT      → return (SomeExpr (Const () ∷ Expr (Stack stkTy) (Stack ((), stkTy))), DynamicType (Proxy ∷ Proxy ((), stkTy)))
        StringUT s  → return (SomeExpr (Const s ∷ Expr (Stack stkTy) (Stack (T.Text, stkTy))), DynamicType (Proxy ∷ Proxy (T.Text, stkTy)))
        BoolUT b    → return (SomeExpr (Const b ∷ Expr (Stack stkTy) (Stack (Bool, stkTy))), DynamicType (Proxy ∷ Proxy (Bool, stkTy)))
        TezUT t     → return (SomeExpr (Const (Tez t) ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), DynamicType (Proxy ∷ Proxy (Tez, stkTy)))
        IntegerUT i → return (SomeExpr (Const i ∷ Expr (Stack stkTy) (Stack (Integer, stkTy))), DynamicType (Proxy ∷ Proxy (Integer, stkTy)))

    ConsPairUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      return (SomeExpr (ConsPair ∷ Expr (Stack (a, (b, c))) (Stack (Pair a b, c))), DynamicType (Proxy ∷ Proxy (Pair a b, c)))

    CarUT → do
      ((DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)), DynamicType (_ ∷ Proxy c)) ← take1Pair
      return (SomeExpr (Car ∷ Expr (Stack (Pair a b, c)) (Stack (a, c))), DynamicType (Proxy ∷ Proxy (a, c)))

    CdrUT → do
      ((DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)), DynamicType (_ ∷ Proxy c)) ← take1Pair
      return (SomeExpr (Cdr ∷ Expr (Stack (Pair a b, c)) (Stack (b, c))), DynamicType (Proxy ∷ Proxy (b, c)))

    IfLeftUT xUT yUT → do
      -- x ty is wrong
      ((DynamicType (Proxy ∷ Proxy xT), DynamicType (Proxy ∷ Proxy yT)), DynamicType (_ ∷ Proxy z)) ← take1Union
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), xEnd) ← liftUntyped xUT (DynamicType (Proxy ∷ Proxy (xT, z))) str
      (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), _)    ← liftUntyped yUT (DynamicType (Proxy ∷ Proxy (yT, z))) str
      case (eqT ∷ Maybe (xF :~: yF), eqT ∷ Maybe (xS :~: (xT, z)), eqT ∷ Maybe (yS :~: (yT, z))) of
        (Just Refl, Just Refl, Just Refl) → return (SomeExpr (IfLeft x y ∷ Expr (Stack (Union xT yT, z)) (Stack xF)), xEnd)
        (Nothing, _, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy xF) (Proxy ∷ Proxy yF))
        (_, Nothing, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy xS) (Proxy ∷ Proxy (xT, z)))
        (_, _, Nothing) → cannotUnify (CannotUnify (Proxy ∷ Proxy yS) (Proxy ∷ Proxy (yT, z)))

    AnnUT LeftUT ty → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)) ← take1
      (DynamicType (Proxy ∷ Proxy lTy), DynamicType (Proxy ∷ Proxy rTy)) ← asUnion (liftType ty)
      case eqT ∷ Maybe (lTy :~: a) of
        Just Refl → return (SomeExpr (Left ∷ Expr (Stack (a, b)) (Stack (Union lTy rTy, b))), DynamicType (Proxy ∷ Proxy (Union lTy rTy, b)))
        Nothing   → cannotUnify (CannotUnify (Proxy ∷ Proxy lTy) (Proxy ∷ Proxy a))

    AnnUT RightUT ty → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)) ← take1
      (DynamicType (Proxy ∷ Proxy lTy), DynamicType (Proxy ∷ Proxy rTy)) ← asUnion (liftType ty)
      case eqT ∷ Maybe (rTy :~: a) of
        Just Refl → return (SomeExpr (Right ∷ Expr (Stack (a, b)) (Stack (Union lTy rTy, b))), DynamicType (Proxy ∷ Proxy (Union lTy rTy, b)))
        Nothing   → cannotUnify (CannotUnify (Proxy ∷ Proxy rTy) (Proxy ∷ Proxy a))

    AddIntIntUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      case (eqT ∷ Maybe (a :~: Integer), eqT ∷ Maybe (b :~: Integer)) of
        (Just Refl, Just Refl) → return (SomeExpr (AddIntInt ∷ Expr (Stack (Integer, (Integer, c))) (Stack (Integer, c))), DynamicType (Proxy ∷ Proxy (Integer, c)))
        (Nothing, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Integer))
        (_, Nothing) → cannotUnify (CannotUnify (Proxy ∷ Proxy b) (Proxy ∷ Proxy Integer))

    MulIntIntUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      case (eqT ∷ Maybe (a :~: Integer), eqT ∷ Maybe (b :~: Integer)) of
        (Just Refl, Just Refl) → return (SomeExpr (MulIntInt ∷ Expr (Stack (Integer, (Integer, c))) (Stack (Integer, c))), DynamicType (Proxy ∷ Proxy (Integer, c)))
        (Nothing, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Integer))
        (_, Nothing) → cannotUnify (CannotUnify (Proxy ∷ Proxy b) (Proxy ∷ Proxy Integer))

    AddTezUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      case (eqT ∷ Maybe (a :~: Tez), eqT ∷ Maybe (b :~: Tez)) of
        (Just Refl, Just Refl) → return (SomeExpr (AddTez ∷ Expr (Stack (Tez, (Tez, c))) (Stack (Tez, c))), DynamicType (Proxy ∷ Proxy (Tez, c)))
        (Nothing, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Tez))
        (_, Nothing) → cannotUnify (CannotUnify (Proxy ∷ Proxy b) (Proxy ∷ Proxy Tez))

    DefaultAccountUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b)) ← take1
      case eqT ∷ Maybe (a :~: Hash) of
        Just Refl → return (SomeExpr (DefaultAccount ∷ Expr (Stack (Hash, b)) (Stack (Contract () (), b))), DynamicType (Proxy ∷ Proxy (Contract () (), b)))
        Nothing   → cannotUnify (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Hash))

    TransferTokensUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), contract@(DynamicType (_ ∷ Proxy c)), DynamicType (_ ∷ Proxy d), DynamicType (_ ∷ Proxy e)) ← take4
      (DynamicType (_ ∷ Proxy paramType), DynamicType (_ ∷ Proxy resultType)) ← asArrow contract
      case (eqT ∷ Maybe (b :~: Tez), eqT ∷ Maybe (a :~: paramType), eqT ∷ Maybe (e :~: ())) of
        (Just Refl, Just Refl, Just Refl) → return (SomeExpr (TransferTokens ∷ Expr (Stack (a, (Tez, (Contract paramType resultType, (d, ()))))) (Stack (resultType, (d, ())))), DynamicType (Proxy ∷ Proxy (resultType, (storageTy, ()))))
        (Nothing, _, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy b) (Proxy ∷ Proxy Tez))
        (_, Nothing, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy paramType))
        (_, _, Nothing) → cannotUnify (CannotUnify (Proxy ∷ Proxy e) (Proxy ∷ Proxy ()))

    SeqUT xUT yUT → do
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), xEnd) ← liftUntyped xUT stk str
      (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), yEnd) ← liftUntyped yUT xEnd str
      case eqT ∷ Maybe (xF :~: yS) of
        Just Refl → return (SomeExpr (Seq x y ∷ Expr (Stack xS) (Stack yF)), yEnd)
        Nothing   → cannotUnify (CannotUnify (Proxy ∷ Proxy xF) (Proxy ∷ Proxy yS))

    IfUT xUT yUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (rest ∷ Proxy b)) ← take1
      case eqT ∷ Maybe (a :~: Bool) of
        Nothing → cannotUnify (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy Bool))
        Just Refl → do
          (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), DynamicType (Proxy ∷ Proxy xEndT)) ← liftUntyped xUT (DynamicType rest) str
          (SomeExpr (y ∷ Expr (Stack yS) (Stack yF)), DynamicType (Proxy ∷ Proxy yEndT)) ← liftUntyped yUT (DynamicType rest) str
          case (eqT ∷ Maybe (xS :~: yS), eqT ∷ Maybe (xF :~: yF), eqT ∷ Maybe (xEndT :~: yEndT)) of
            (Just Refl, Just Refl, Just Refl) → return (SomeExpr (If x y), DynamicType (Proxy ∷ Proxy xEndT))
            (Nothing, _, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy xS) (Proxy ∷ Proxy yS))
            (_, Nothing, _) → cannotUnify (CannotUnify (Proxy ∷ Proxy xF) (Proxy ∷ Proxy yF))
            (_, _, Nothing) → cannotUnify (CannotUnify (Proxy ∷ Proxy xEndT) (Proxy ∷ Proxy yEndT))

    DipUT xUT → do
      (DynamicType (_ ∷ Proxy a), r@(DynamicType (_ ∷ Proxy b))) ← take1
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), _) ← liftUntyped xUT r str
      case eqT ∷ Maybe (xS :~: b) of
        Just Refl → return (SomeExpr (Dip x ∷ Expr (Stack (a, b)) (Stack (a, xF))), DynamicType (Proxy ∷ Proxy (a, xF)))
        Nothing   → cannotUnify (CannotUnify (Proxy ∷ Proxy xS) (Proxy ∷ Proxy b))

    {-

    LambdaUT xUT → do
      (SomeExpr (x ∷ Expr (Stack xS) (Stack xF)), _) ← liftUntyped xUT (SomeStack Empty) {- TODO: Magic stack type. -}
      return (SomeExpr (Lambda (LambdaW x) ∷ Expr (Stack stkTy) (Stack (Lambda xS xF, stkTy))), SomeStack (Item (undefined ∷ Lambda xS xF) stack))

    -}

    -- Deal with annotations here?
    FailUT  → return (SomeExpr (Fail ∷ Expr (Stack stkTy) (Stack ())), stk)

    NopUT   → return (SomeExpr (Nop ∷ Expr (Stack stkTy) (Stack stkTy)), stk)

    CompareUT → do
      (DynamicType (_ ∷ Proxy a), DynamicType (_ ∷ Proxy b), DynamicType (_ ∷ Proxy c)) ← take2
      case eqT ∷ Maybe (a :~: b) of
        Just Refl → return (SomeExpr (Compare ∷ Expr (Stack (a, (a, c))) (Stack (Integer, c))), DynamicType (Proxy ∷ Proxy (Integer, c)))
        Nothing   → cannotUnify (CannotUnify (Proxy ∷ Proxy a) (Proxy ∷ Proxy b))

    NowUT       → return (SomeExpr (Now ∷ Expr (Stack stkTy) (Stack (Timestamp, stkTy))), DynamicType (Proxy ∷ Proxy (Timestamp, stkTy)))
    BalanceUT   → return (SomeExpr (Balance ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), DynamicType (Proxy ∷ Proxy (Tez, stkTy)))
    AmountUT    → return (SomeExpr (Amount ∷ Expr (Stack stkTy) (Stack (Tez, stkTy))), DynamicType (Proxy ∷ Proxy (Tez, stkTy)))

    AnnUT e _ → liftUntyped e stk str

    _ → notImplemented

{-  Lift a Michelson type into a Haskell type existential.  -}

liftType ∷ Type → DynamicType
liftType UnitT          = toDynamicType (Proxy ∷ Proxy ())
liftType KeyT           = toDynamicType (Proxy ∷ Proxy Key)
liftType HashT          = toDynamicType (Proxy ∷ Proxy Hash)
liftType IntT           = toDynamicType (Proxy ∷ Proxy Integer)
liftType TezT           = toDynamicType (Proxy ∷ Proxy Tez)
liftType BoolT          = toDynamicType (Proxy ∷ Proxy Bool)
liftType StringT        = toDynamicType (Proxy ∷ Proxy T.Text)
liftType (EitherT a b)  =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy a), DynamicType (Proxy ∷ Proxy b)) → toDynamicType (Proxy ∷ Proxy (Union a b))
liftType (OptionT a) =
  case liftType a of
    DynamicType (Proxy ∷ Proxy a) → toDynamicType (Proxy ∷ Proxy (Option a))
liftType (ListT a) =
  case liftType a of
    DynamicType (Proxy ∷ Proxy a) → toDynamicType (Proxy ∷ Proxy (List a))
liftType (PairT a b) =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy x), DynamicType (Proxy ∷ Proxy y)) → toDynamicType (Proxy ∷ Proxy (Pair x y))
liftType (LamT a b) =
  case (liftType a, liftType b) of
    (DynamicType (Proxy ∷ Proxy x), DynamicType (Proxy ∷ Proxy y)) → toDynamicType (Proxy ∷ Proxy (x → y))

typeToStack ∷ Type → DynamicType
typeToStack ty =
  case liftType ty of
    DynamicType (Proxy ∷ Proxy t) → DynamicType (Proxy ∷ Proxy (t, ()))
