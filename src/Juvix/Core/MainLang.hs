-- Quantitative type implementation following
-- Atkey 2017 and McBride 2016.
module Juvix.Core.MainLang where

import           Prelude
import           Numeric.Natural

-- Inferable terms
data ITerm
  =  Star Natural             -- (sort i) i th ordering of (closed) universe.
  |  Natural                  -- (Prim) primitive type
  |  Pi Natural CTerm CTerm   -- formation rule of the dependent function type (PI). 
                              -- the Natural(π) tracks how many times x is used.
  |  Pm Natural CTerm CTerm   -- dependent multiplicative conjunction (tensor product)
  |  Pa Natural CTerm CTerm   -- dependent additive conjunction type
  |  NPm CTerm CTerm          -- non-dependent multiplicative disjunction type
  |  Bound Natural            -- Bound variables, in de Bruijn indices
  |  Free Name                -- Free variables of type name (see below)
  |  App Natural ITerm CTerm  -- elimination rule of PI (APP).
                              -- the Natural(π) tracks how x is use.
  deriving (Show, Eq)

--Checkable terms
data CTerm
  =  Inf  ITerm         --(CONV) conversion rule. TODO make sure 0Γ ⊢ S≡T
                        -- Inf is the constructor that embeds ITerm to CTerm
  |  Lam  Natural CTerm --(LAM) Introduction rule of PI.
                        -- The abstracted variable's usage is tracked with the Natural(π).
  deriving (Show, Eq)

data Name
  =  Global  String -- Global variables are represented by name thus type string
  |  Local   Natural    -- to convert a bound variable into a free one
  |  Quote   Natural
  deriving (Show, Eq)

--Values/types
data Value
  = VLam (Value -> Value)
  | VStar Natural
  | VPi Natural Value (Value -> Value)
  | VPm Natural Value (Value -> Value)
  | VPa Natural Value (Value -> Value)
  | VNPm Value Value
  | VNeutral Neutral

--A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value

showVal :: Value → String
showVal (VLam _)      = "lambda"
showVal (VStar i)     = "* " ++ show i
showVal (VPi n v _f)  = "Pi " ++ show n ++ showVal v ++ " -> lambda"
showVal (VNeutral _n) = "neutral "

--vfree creates the value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

--Contexts map variables to their types.
type Type    = Value
type Context = [(Name, Type)]

toInt :: Natural → Int
toInt = fromInteger . toInteger

--Evaluation
type Env = [Value]

iEval :: ITerm → Env → Value
iEval (Star i)     _d  =  VStar i
iEval (Pi n ty ty') d  =  VPi (n-1) (cEval ty d) (\ x -> cEval ty' (x : d))
iEval (Pm n ty ty') d  =  VPm (n-1) (cEval ty d) (\ x -> cEval ty' (x : d))
iEval (Pa n ty ty') d  =  VPa (n-1) (cEval ty d) (\ x -> cEval ty' (x : d))
iEval (Free  x)    _d  =  vfree x
iEval (Bound  ii)   d  =  d !! (toInt ii) --(!!) :: [a] -> Int -> a, the list lookup operator.
iEval (App iterm cterm)   d  =  vapp (iEval iterm d) (cEval cterm d)
iEval Natural       _  = undefined
iEval (NPm  ty ty') d  =  undefined VNPm ty ty' d

vapp :: Value → Value → Value
vapp (VLam f)      v =  f v
vapp (VNeutral n)  v =  VNeutral (NApp n v)
vapp x             y =  error ("Application (vapp) error. Cannot apply \n" ++ showVal y ++ "\n to \n" ++ showVal x)

cEval :: CTerm → Env → Value
cEval (Inf  ii)   d =  iEval ii d
cEval (Lam  e)    d =  VLam (\ x -> cEval e (x : d))

--substitution function for inferable terms
iSubst :: Natural → ITerm → ITerm → ITerm
iSubst ii r (Bound j)
  | ii == j  = r
  |otherwise = Bound j
iSubst _ii _r (Free y)        =  Free y
--iSubst ii r (App iterm cterm) =
iSubst _ii _r (Star i)        =  Star i
--iSubst ii r (Pi n ty ty')     =  Pi

--substitution function for checkable terms
cSubst :: Natural -> ITerm -> CTerm -> CTerm
cSubst ii r (Inf e) =  Inf (iSubst ii r e)
cSubst ii r (Lam e) =  Lam (cSubst (ii + 1) r e)
