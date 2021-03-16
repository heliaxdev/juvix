module Juvix.Core.IR.Typechecker.CheckDatatype
  ( module Juvix.Core.IR.Typechecker.CheckDatatype,
  )
where

import Juvix.Core.IR.CheckTerm
import qualified Juvix.Core.IR.Evaluator as Eval
-- import SPos ( sposConstructor )
import Juvix.Core.IR.Types.Base
import Juvix.Core.IR.Types.Globals
import qualified Data.Map.Internal as Map
import Juvix.Library ()

typeCheckConstructor ::
     Name -> [Pos] -> RawTelescope ext primTy primVal -> (Name, Term' ext primTy primVal) -> TypeCheck ty ext primTy primVal ()
typeCheckConstructor name pos tel (n, ty) =
  pure ()
  -- sig <- get -- get signatures
  -- let tt = teleToType tel t
  --     params = length tel
  -- _ <- checkConType 0 [] [] params tt
  -- let (_, target) = typeToTele tt
  -- checkTarget name tel target
  -- vt <- eval [] tt
  -- -- sposConstructor name 0 pos vt -- strict positivity check
  -- put (addSig sig n (ConSig vt))

-- teleToType :: Telescope -> Expr -> Expr
-- teleToType [] t            = t
-- teleToType ((n, t):tel) t2 = Pi n t (teleToType tel t2)

-- typeToTele :: Expr -> (Telescope, Expr)
-- typeToTele t = ttt t []
--   where
--     ttt :: Expr -> Telescope -> (Telescope, Expr)
--     ttt (Pi n t' t2) tel = ttt t2 (tel <> [(n, t')])
--     ttt x tel            = (tel, x)

-- -- | checkDataType takes 5 arguments.
-- -- 1st argument is the next fresh generic value.
-- -- 2nd argument is an env that binds fresh generic values to variables.
-- -- 3rd argument is an env that binds the type value corresponding to these generic values.
-- -- 4th argument is the length of the telescope, or the no. of parameters.
-- -- 5th argument is the expression that is left to be checked.
-- checkDataType :: Int -> Env -> Env -> Int -> Expr -> TypeCheck ()
-- checkDataType k rho gamma p (Pi x t1 t2) = do
--   _ <-
--     if k < p -- if k < p then we're checking the parameters
--       then checkType k rho gamma t1 -- checks params are valid types
--       else checkSType k rho gamma t1 -- checks arguments Θ are Star types
--   v_t1 <- eval rho t1
--   checkDataType (k + 1) (updateEnv rho x (VGen k)) (updateEnv gamma x v_t1) p t2
-- -- check that the data type is of type Star
-- checkDataType _k _rho _gamma _p Star = return ()
-- checkDataType _k _rho _gamma _p e =
--   error $ "checkDataType: " <> show e <> "doesn't target Star."

-- -- | checkConType check constructor type
-- -- 1st argument is the next fresh generic value.
-- -- 2nd argument is an env that binds fresh generic values to variables.
-- -- 3rd argument is an env that binds the type value corresponding to these generic values.
-- -- 4th argument is the length of the telescope, or the no. of parameters.
-- -- 5th argument is the expression that is left to be checked.
-- checkConType :: Int -> Env -> Env -> Int -> Expr -> TypeCheck ()
-- checkConType k rho gamma p e =
--   case e of
--     Pi x t1 t2 -> do
--       if k < p
--         then return () -- params were already checked by checkDataType
--         else checkSType k rho gamma t1 -- check that arguments ∆ are stypes
--       v_t1 <- eval rho t1
--       checkConType
--         (k + 1)
--         (updateEnv rho x (VGen k))
--         (updateEnv gamma x v_t1)
--         p
--         t2
--     -- the constructor's type is of type Star(the same type as the data type).
--     _ -> checkExpr k rho gamma e VStar 
    

-- -- check that the data type and the parameter arguments
-- -- are written down like declared in telescope
-- checkTarget :: Name -> Telescope -> Expr -> TypeCheck ()
-- checkTarget name tel tg@(App (Def n) al) =
--   if n == name
--     then do
--       let pn = length tel
--           params = take pn al
--       checkParams tel params -- check parameters
--     else error $
--          "checkTarget: target mismatch " <> show tg <> ". Input name is " <>
--          show name <>
--          ". Input telescope is " <>
--          show tel
-- checkTarget name tel tg@(Def n) =
--   if n == name && null tel
--     then return ()
--     else error $
--          "checkTarget: target mismatch" <> show tg <> ". Input name is " <>
--          show name <>
--          ". Input telescope is " <>
--          show tel
-- checkTarget name tel tg =
--   error $
--   "checkTarget: target mismatch" <> show tg <> ". Input name is " <> show name <>
--   ". Input telescope is " <>
--   show tel

-- -- check parameters
-- checkParams :: Telescope -> [Expr] -> TypeCheck ()
-- checkParams [] [] = return ()
-- checkParams tel@((n, _t):tl) (Var n':el) =
--   if n == n'
--     then checkParams tl el
--     else error $
--          "checkParams: target parameter mismatch. The input telescope is " <>
--          show tel <>
--          ". One of the name in the telescope is " <>
--          show n <> -- using show to wrap n with "
--          ", which does not match the input expression's variable name: " <>
--          show n'
-- checkParams _ exps =
--   error $
--     "checkParams: target parameter mismatch. The input expression"
--     <> show exps
--     <> "isn't a variable (Var)."