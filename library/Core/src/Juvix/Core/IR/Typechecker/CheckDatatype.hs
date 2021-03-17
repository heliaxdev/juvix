module Juvix.Core.IR.Typechecker.CheckDatatype
  ( module Juvix.Core.IR.Typechecker.CheckDatatype,
  )
where

import Juvix.Core.IR.CheckTerm
import qualified Juvix.Core.IR.Evaluator as Eval
-- import SPos ( sposConstructor )
import Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.Types.Globals as IR
import Juvix.Library

typeCheckConstructor ::
  HasState "typeSigs" s IO =>
  Name ->
  [IR.Pos] ->
  RawTelescope ext primTy primVal ->
  (IR.Name, IR.Term' ext primTy primVal) ->
  TypeCheck ext primTy primVal IO ()
typeCheckConstructor name pos tel (n, ty) = do
  sig <- get @"typeSigs" -- get signatures
  let (n, t) = teleToType tel ty
      params = length tel
  -- _ <- checkConType 0 [] [] params tt
  let (_, target) = typeToTele (n, t)
  -- checkTarget name tel target
  -- vt <- eval [] tt
  -- sposConstructor name 0 pos vt -- strict positivity check
  -- put (addSig sig n (ConSig vt))
  return ()

teleToType ::
  RawTelescope ext primTy primVal ->
  IR.Term' ext primTy primVal ->
  (Maybe Name, IR.Term' ext primTy primVal)
teleToType [] t = (Nothing, t)
teleToType ((n, t) : tel) t2 = undefined

-- TODO(Just n, Pi Omega t (snd (teleToType tel t2)) ext?)

typeToTele :: (Maybe Name, IR.Term' ext primTy primVal) -> (RawTelescope ext primTy primVal, IR.Term' ext primTy primVal)
typeToTele (n, t) = ttt (n, t) []
  where
    ttt ::
      (Maybe Name, IR.Term' ext primTy primVal) ->
      RawTelescope ext primTy primVal ->
      (RawTelescope ext primTy primVal, IR.Term' ext primTy primVal)
    ttt (Just n, Pi usage t' t2 _) tel =
      ttt (Nothing, t2) (tel <> [(n, t')]) --TODO t2 name?
    ttt x tel = (tel, snd x)

-- -- | checkDataType takes 5 arguments.
-- -- 1st argument is the next fresh generic value.
-- -- 2nd argument is an env that binds fresh generic values to variables.
-- -- 3rd argument is an env that binds the type value corresponding to these generic values.
-- -- 4th argument is the length of the telescope, or the no. of parameters.
-- -- 5th argument is the expression that is left to be checked.
-- checkDataType :: Int -> Env -> Env -> Int -> IR.Term' ext primTy primVal -> TypeCheck ()
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
-- checkConType :: Int -> Env -> Env -> Int -> IR.Term' ext primTy primVal -> TypeCheck ()
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
-- checkTarget :: Name -> RawTelescope ext primTy primVal -> IR.Term' ext primTy primVal -> TypeCheck ()
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
-- checkParams :: RawTelescope ext primTy primVal -> [IR.Term' ext primTy primVal] -> TypeCheck ()
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
