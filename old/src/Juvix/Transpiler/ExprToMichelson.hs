module Juvix.Transpiler.ExprToMichelson (
  exprToMichelson
) where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Text                 as T
import           Foundation
import qualified Prelude                   as P
import           Safe                      (readMay)

import qualified Juvix.Backends.Michelson  as M
import           Juvix.Core
import           Juvix.Core.CompilerTypes
import           Juvix.Transpiler.Encoding
import           Juvix.Transpiler.Utility

{-  Stage 3 : Transform simplified intermediary expression to Michelson instruction sequence.

    This requires tracking the state of the stack through the Michelson evaluation path.

    At the moment, this function maintains an invariant forward mapping between the Haskell type of the expression and the type of the Michelson stack.
    Commented on each case statement: ∷ { Haskell Type } ~ { Stack Pre-Evaluation } ⇒ { Stack Post-Evaluation }   -}

exprToMichelson ∷ Expr M.Type → CompilerM M.ExprUT M.Type
exprToMichelson expr = do
  beg ← get
  let tellReturn ∷ M.ExprUT → CompilerM M.ExprUT M.Type
      tellReturn ret = do
        end ← get
        tell [ExprToMichelson expr (prettyPrintValue ret) beg end]
        return ret
  case expr of

    Ann expr ty → do
      expr ← exprToMichelson expr
      return (M.AnnUT expr ty)

    -- (special)
    -- TODO Deal with compile-time known stack type.
    BuiltIn s | readMay (T.unpack s) == Just M.TransferTokensUT → do
      modify (drop 4)
      count ← P.length |<< get
      modify ((:) FuncResult)
      let foldDrop 0 = M.NopUT
          foldDrop n = M.SeqUT M.DropUT (foldDrop (n - 1))
          foldAdd  0 = M.NopUT
          foldAdd  1 = M.SeqUT (M.SeqUT (M.ConstUT M.UnitUT) (M.ConstUT M.UnitUT)) M.ConsPairUT
          foldAdd  n = M.SeqUT (M.ConstUT M.UnitUT) (foldAdd (n - 1))
          clear      = M.DipUT (M.DipUT (M.DipUT (M.DipUT (foldDrop count))))
      tellReturn (M.SeqUT clear (M.SeqUT M.TransferTokensUT (M.SeqUT M.ConsPairUT (M.DipUT (foldAdd count)))))

    -- (various)
    BuiltIn b ->
      case readMay (T.unpack b) of
        Just e  → do
          genReturn e
        Nothing → throwError (NotYetImplemented ("exprToMichelson: BuiltIn: " `T.append` b))

    -- ∷ a ~ s ⇒ (a, s)
    Lit l → do
      genReturn (litToExpr l)

    -- ∷ a ~ s ⇒ (a, s)
    Var v → do
      stack ← get
      case position v stack of
        Nothing → throwError (VariableNotInScope v stack)
        Just i  → do
          let before  = if i == 0 then M.NopUT else rearrange i
              after   = if i == 0 then M.NopUT else M.DipUT (unrearrange i)
          genReturn (M.SeqUT (M.SeqUT before M.DupUT) after)

    -- ∷ \a → b ~ (a, s) ⇒ (b, s)
    Lam v e → do
      -- TODO: Check if arguments used and drop unused ones before function call, proper occurence analysis.
      -- Relabel arguments on the stack when the function is called according to calling convention.
      let collect e vs =
            case e of
              Lam v e → collect e (vs <> [v])
              e       → do
                modify ((<>) (fmap BoundVariable vs) . P.drop (P.length vs))
                inner ← exprToMichelson e
                after ← genReturn (foldDrop (P.length vs))
                tellReturn (M.SeqUT inner after)
      collect e [v]

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    App x y → do
      ycomp ← exprToMichelson y
      xcomp ← exprToMichelson x
      return (M.SeqUT ycomp xcomp)

    -- Should be eliminated prior to this stage.
    Let _ _ _ → throwError (NotYetImplemented ("exprToMichelson: " `T.append` prettyPrintValue expr))

    -- ∷ IO a >>= (\a → IO b) ~ s ⇒ (b, s)
    BindIO x y → exprToMichelson (App y x)

    -- ∷ IO a >> IO b ~ s ⇒ (b, s)
    SeqIO x y → do
      x ← exprToMichelson x
      modify (drop 1)
      y ← exprToMichelson y
      return (M.SeqUT (M.SeqUT x M.DropUT) y)

    -- ∷ IO a ~ s ⇒ (a, s)
    ReturnIO e → exprToMichelson e

    -- ∷ a ~ s ⇒ (a, s)
    -- Rewrite >2 case alternatives
    Case scrutinee maybeBinder scrutineeType (one:two:three:rest) → do
      start ← get
      expr ← exprToMichelson (Case scrutinee maybeBinder scrutineeType [
        one,
        DefaultCase (Case scrutinee maybeBinder scrutineeType (two:three:rest))
        ])
      end ← get
      unless (P.drop 1 end == start) (throwError (NotYetImplemented (T.concat ["Case compilation (>2) violated stack invariant: ", prettyPrintValue expr, "; starting stack ", T.intercalate "," (fmap prettyPrintValue start), ", ending stack ", T.intercalate "," (fmap prettyPrintValue end)])))
      return expr

    -- ∷ a ~ s ⇒ (a, s)
    Case scrutinee maybeBinder scrutineeType alternatives → do
      -- Special-case bool, either, maybe
      -- Evaluate binder, bind as necessary
      -- Generate switch on native repr (never constructor tag except for e.g. data Ord = A | B | C)
      -- Unpack necessary variables in fixed pattern according to desired bindings
      -- (later: lazy unpack? or remove in optimization stage any unused bindings?)
      -- Rewrite case alternatives with more than two alternatives to nested version?

      start ← get

      let invariantGuard = do
            end ← get
            unless (P.drop 1 end == start) (throwError (NotYetImplemented (T.concat ["Case compilation violated stack invariant: ", prettyPrintValue expr, "; starting stack ", T.intercalate "," (fmap prettyPrintValue start), ", ending stack ", T.intercalate "," (fmap prettyPrintValue end)])))

      {- Evaluate scrutinee. -}
      scrutinee ← exprToMichelson scrutinee

      {- Bind scrutinee. -}
      (binderDup, binderDrop) ← do
        case maybeBinder of
          Just binder → do
            modify ((:) (BoundVariable binder) . drop 1)
            return (M.DupUT, M.SeqUT M.SwapUT M.DropUT)
          Nothing     → do
            return (M.NopUT, M.NopUT)

      let binderDupM  = modify =<< genFunc binderDup
          binderDropM = modify =<< genFunc binderDrop

      case alternatives of

        [DefaultCase expr, CaseOption conB bindsB caseB] → do
          switch ← genSwitch scrutineeType
          switchCase ← do
            binderDupM
            now ← get
            unpackA ← exprToMichelson (BuiltIn "DropUT")
            exprA ← exprToMichelson expr
            modify (const now)
            unpackB ← unpack (conRepType conB) bindsB
            caseB ← exprToMichelson caseB
            unpackDropB ← unpackDrop bindsB
            let a = M.SeqUT unpackA exprA
                b = M.SeqUT (M.SeqUT unpackB caseB) unpackDropB
            return (switch a b)
          binderDropM
          invariantGuard
          return (M.SeqUT (M.SeqUT scrutinee binderDup) (M.SeqUT switchCase binderDrop))

        [CaseOption conA bindsA caseA, DefaultCase expr] → do
          switch ← genSwitch scrutineeType
          switchCase ← do
            binderDupM
            now ← get
            unpackA ← unpack (conRepType conA) bindsA
            caseA ← exprToMichelson caseA
            unpackDropA ← unpackDrop bindsA
            modify (const now)
            unpackB ← exprToMichelson (BuiltIn "DropUT")
            exprB ← exprToMichelson expr
            let a = M.SeqUT (M.SeqUT unpackA caseA) unpackDropA
                b = M.SeqUT unpackB exprB
            return (switch a b)
          binderDropM
          invariantGuard
          return (M.SeqUT (M.SeqUT scrutinee binderDup) (M.SeqUT switchCase binderDrop))

        [CaseOption conA bindsA caseA, CaseOption conB bindsB caseB] → do
          switch ← genSwitch scrutineeType

          -- TODO: Constructor ordering?
          {-
          if conTag conA == fst && conTag conB == snd then do
            return (switch caseA caseB)
          else if conTag conA == snd && conTag conB == fst then do
            return (switch (M.SeqUT unpackB caseB) (M.SeqUT unpackA caseA))
          else throwError (NotYetImplemented (T.concat ["exprToMichelson (tag mismatch): ", conTag conA, " / ", conTag conB, " /= ", fst, " / ", snd, " in ", prettyPrintValue expr]))
          -}

          switchCase ← do
            binderDupM
            now ← get
            unpackA ← unpack (conRepType conA) bindsA
            caseA ← exprToMichelson caseA
            unpackDropA ← unpackDrop bindsA
            modify (const now)
            unpackB ← unpack (conRepType conB) bindsB
            caseB ← exprToMichelson caseB
            unpackDropB ← unpackDrop bindsB
            let a = M.SeqUT (M.SeqUT unpackA caseA) unpackDropA
                b = M.SeqUT (M.SeqUT unpackB caseB) unpackDropB
            return (switch a b)

          binderDropM
          invariantGuard
          return (M.SeqUT (M.SeqUT scrutinee binderDup) (M.SeqUT switchCase binderDrop))

        (CaseOption _ binds exprA):[] → do
          binderDupM
          one ← get
          unpack ← unpack scrutineeType binds
          two ← get
          exprA ← exprToMichelson exprA
          three ← get
          unpackDrop ← unpackDrop binds
          four ← get
          binderDropM
          five ← get
          unless (P.drop 1 five == start)
            (throwError (NotYetImplemented (T.concat ["caught on ", prettyPrintValue expr, " @ ", prettyPrintValue start, " <> ", prettyPrintValue one, " <> ", prettyPrintValue two, " <> ", prettyPrintValue three, " <> ", prettyPrintValue four, " <> ", prettyPrintValue five])))
          --invariantGuard
          return (M.SeqUT (M.SeqUT (M.SeqUT scrutinee binderDup) (M.SeqUT (M.SeqUT unpack exprA) unpackDrop)) binderDrop)

        (DefaultCase expr):[] → do
          {- This should probably be optimized away. -}
          binderDupM
          expr ← exprToMichelson expr
          binderDropM
          invariantGuard
          return (M.SeqUT (M.SeqUT scrutinee binderDup) (M.SeqUT expr binderDrop))

        _ → do
          throwError (NotYetImplemented ("exprToMichelson: " `T.append` prettyPrintValue expr))

litToExpr ∷ Literal → M.ExprUT
litToExpr LUnit     = M.ConstUT M.UnitUT
litToExpr (LNat l)  = M.ConstUT (M.IntegerUT l)
litToExpr (LInt l)  = M.ConstUT (M.IntegerUT l)
litToExpr (LTez t)  = M.ConstUT (M.TezUT t)
litToExpr (LBool b) = M.ConstUT (M.BoolUT b)

genSwitch ∷ M.Type → CompilerM (M.ExprUT → M.ExprUT → M.ExprUT) M.Type
genSwitch M.BoolT           = return M.IfUT
genSwitch (M.EitherT _ _)   = return M.IfLeftUT
genSwitch (M.OptionT _)     = return M.IfNoneUT
genSwitch (M.ListT _)       = return M.IfConsUT
genSwitch ty                = throwError (NotYetImplemented ("genSwitch: " `T.append` prettyPrintValue ty))

{-
orderSwitch ∷ M.Type → CompilerM (T.Text, T.Text)
orderSwitch M.BoolT         = return ("True", "False")
orderSwitch (M.EitherT _ _) = return ("Left", "Right")
orderSwitch (M.OptionT _)   = return ("None", "Some")
orderSwitch (M.ListT _)     = return ("Cons", "Nil")
orderSwitch ty              = throwError (NotYetImplemented ("orderSwitch: " `T.append` prettyPrintValue ty))
-}
