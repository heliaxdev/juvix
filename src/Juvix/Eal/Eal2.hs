module Juvix.Eal.Eal2 where

import qualified Data.Map.Strict  as Map
import           Juvix.Eal.Types2
import           Juvix.Library    hiding (Type, link, reduce)
import           Prelude                 (error)

{- Main functionality. -}

-- Construct occurrence map.
setOccurrenceMap ∷ (HasState "occurrenceMap" OccurrenceMap m) ⇒ Term → m ()
setOccurrenceMap term = do
  case term of
    Var sym →
      modify' @"occurrenceMap" (Map.insertWith (+) sym 1)
    Lam _ b → do
      setOccurrenceMap b
    App a b → do
      setOccurrenceMap a
      setOccurrenceMap b

-- Parameterize type.
parameterizeType :: ( HasState  "nextParam" Param          m
                   , HasWriter "constraints" [Constraint] m )
                 ⇒ Type → m PType
parameterizeType ty = do
  param ← freshParam
  -- Typing constraint: m >= 0
  addConstraint (Constraint [ConstraintVar 1 param] (Gte 0))
  case ty of
    SymT sym →
      pure (PSymT param sym)
    ArrT arg body → do
      arg  ← parameterizeType arg
      body ← parameterizeType body
      pure (PArrT param arg body)

-- Parameterize type assignment.
parameterizeTypeAssignment :: ( HasState "nextParam" Param               m
                             , HasWriter "constraints" [Constraint]     m
                             , HasState "typeAssignment" TypeAssignment m )
                           ⇒ m ParamTypeAssignment
parameterizeTypeAssignment = do
  assignment ← get @"typeAssignment"
  traverse parameterizeType assignment

-- Reparameterize.
reparameterize ∷ ( HasState "nextParam" Param           m
                 , HasWriter "constraints" [Constraint] m )
               ⇒ PType → m PType
reparameterize pty = do
  param ← freshParam
  -- Typing constraint: m >= 0
  addConstraint (Constraint [ConstraintVar 1 param] (Gte 0))
  case pty of
    PSymT _ sym → pure (PSymT param sym)
    PArrT _ a b → pure (PArrT param a b)

unificationConstraints ∷ (HasWriter "constraints" [Constraint] m)
                       ⇒ PType → PType → m ()
unificationConstraints (PSymT a _) (PSymT b _) =
  addConstraint (Constraint [ConstraintVar 1 a, ConstraintVar (-1) b] (Eq 0))
unificationConstraints (PArrT a aArg aRes) (PArrT b bArg bRes) = do
  addConstraint (Constraint [ConstraintVar 1 a, ConstraintVar (-1) b] (Eq 0))
  unificationConstraints aArg bArg
  unificationConstraints aRes bRes

unificationConstraints PSymT {} PArrT {} = error "doesn't happen"
unificationConstraints PArrT {} PSymT {} = error "doesn't happen"

-- Generate boxing & typing constraints.
-- In one pass to avoid keeping extra maps.
boxAndTypeConstraint :: ( HasState  "path" Path                    m
                       , HasState  "varPaths" VarPaths            m
                       , HasState  "nextParam" Param              m
                       , HasWriter "constraints" [Constraint]     m
                       , HasState  "occurrenceMap" OccurrenceMap  m )
                     ⇒ ParamTypeAssignment
                     → Term
                     → m (RPT, PType)
boxAndTypeConstraint parameterizedAssignment term = do
  let rec = boxAndTypeConstraint parameterizedAssignment
  varPaths ← get @"varPaths"
  param    ← addPath
  path     ← get @"path"
  addConstraint (Constraint (ConstraintVar 1 <$> path) (Gte 0))
  case term of
    Var sym → do
      -- Boxing constraint.
      case varPaths Map.!? sym of
        Just loc  → addConstraint (Constraint (ConstraintVar 1 <$> dropWhile (< loc) path) (Eq 0))
        Nothing   → addConstraint (Constraint (ConstraintVar 1 <$> path)                   (Eq 0))
      let paramTy = parameterizedAssignment Map.! sym
      -- Typing constraint: variable occurrences.
      occurrenceMap ← get @"occurrenceMap"

      let occurrences   = occurrenceMap Map.! sym
          origBangParam = bangParam paramTy

      when (occurrences >= 2) $
        addConstraint (Constraint [ConstraintVar 1 origBangParam] (Gte 1))
      -- Calculate parameterized type for subterm.
      paramTy ← reparameterize paramTy
      -- Typing constraint: m = k + n, m >= 0; n = param, m = bangParam paramTy, k = origBangParam
      addConstraint (Constraint [ ConstraintVar (-1) (bangParam paramTy)
                                , ConstraintVar 1 origBangParam
                                , ConstraintVar 1 param
                                ] (Eq 0))
      -- Return parameterized term.
      pure (RBang param (RVar sym), paramTy)
    Lam sym body → do
      modify' @"varPaths" (Map.insert sym (succ param))
      (body, bodyTy) ← rec body
      -- Calculate parameterized type for subterm.
      lamTyParam ← freshParam
      -- Typing constraint: m >= 0
      addConstraint (Constraint [ConstraintVar 1 lamTyParam] (Gte 0))

      let argTy = parameterizedAssignment Map.! sym
          lamTy = PArrT lamTyParam argTy bodyTy
      -- Calculate final type.
      resTy ← reparameterize lamTy
      -- Typing contraint: m = 0
      addConstraint (Constraint [ConstraintVar 1 lamTyParam] (Eq 0))
      -- Typing constraint: m = k + n, m >= 0, n = param, m = bangParam resTy, k = bangParam lamTy
      addConstraint (Constraint [ ConstraintVar (-1) (bangParam resTy)
                                , ConstraintVar 1 (bangParam lamTy)
                                , ConstraintVar 1 param
                                ] (Eq 0))
      -- Return parameterized term.
      pure (RBang param (RLam sym body), resTy)
    App a b → do
      (a, aTy) ← rec a
      let PArrT _ argTy resTy = aTy
      put @"path" path
      put @"varPaths" varPaths
      (b, bTy) ← rec b
      -- Calculate parameterized type for subterm.
      appTy ← reparameterize resTy
      -- Typing constraint: U(A_1, A_2), m = 0
      unificationConstraints argTy bTy
      addConstraint (Constraint [ConstraintVar 1 (bangParam aTy)] (Eq 0))
      -- Typing constraint: m = k + n, m >= 0, n = param, m = bangParam appTy, k = bangParam resTy
      addConstraint (Constraint [ ConstraintVar (-1) (bangParam appTy)
                                , ConstraintVar 1 (bangParam resTy)
                                , ConstraintVar 1 param
                                ] (Eq 0))
      -- Return parameterized term.
      pure (RBang param (RApp a b), appTy)

-- Generate constraints.
generateConstraints ∷ ( HasState  "path"           Path            m
                      , HasState  "varPaths"       VarPaths        m
                      , HasState  "nextParam"      Param           m
                      , HasState  "typeAssignment" TypeAssignment  m
                      , HasWriter "constraints"    [Constraint]    m
                      , HasState  "occurrenceMap"  OccurrenceMap   m )
                    ⇒ Term → m RPT
generateConstraints term = do
  parameterizedAssignment ← parameterizeTypeAssignment
  setOccurrenceMap term
  boxAndTypeConstraint parameterizedAssignment term
  >>| fst

{- Bracket Checker. -}
bracketChecker :: RPTO → Either BracketErrors ()
bracketChecker t = runEither (rec' t 0 mempty)
  where
    rec' (RBang changeBy (RVar sym)) n map =
      let f x
            | changeBy + n + x == 0 = pure ()
            | changeBy + n + x >  0 = throw @"typ" TooManyOpenV
            | otherwise             = throw @"typ" TooManyClosingV
      in case Map.lookup sym map of
        Just x  → f x
        Nothing → f 0
    rec' (RBang changeBy t) n map
      | changeBy + n < 0 = throw @"typ" TooManyClosing
      | otherwise =
        let n' = changeBy + n
        in case t of
          RLam s t   → rec' t n' (Map.insert s (negate n') map)
          RApp t1 t2 → rec' t1 n' map >> rec' t2 n' map
          RVar _     → error "already is matched"

{- Utility. -}

-- The outer bang parameter.
bangParam ∷ PType → Param
bangParam (PSymT param _)   = param
bangParam (PArrT param _ _) = param

-- Generate fresh parameter.
freshParam ∷ (HasState "nextParam" Param m) ⇒ m Param
freshParam = do
  param ← get @"nextParam"
  put @"nextParam" (succ param)
  pure param

-- Append to path.
addPath ∷ (HasState "nextParam" Param m, HasState "path" Path m) ⇒ m Param
addPath = do
  param ← freshParam
  modify' @"path" (<> [param])
  pure param

-- Add constraint.
addConstraint ∷ HasWriter "constraints" [Constraint] m ⇒ Constraint → m ()
addConstraint con = tell @"constraints" [con]

-- Execute with prior assignment.
execWithAssignment ∷ TypeAssignment → EnvConstraint a → (a, Env)
execWithAssignment assignment (EnvCon env) =
  runState env (Env [] mempty assignment 0 [] mempty)
