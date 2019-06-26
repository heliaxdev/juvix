--Dependent type implementation following 
--"A tutorial implementation of a dependently typed lambda calculus"

import Control.Monad.Except --enable throwError 

-- Inferable terms 
data ITerm 
  =  Ann    CTerm CTerm --annotated terms
  |  Star --(STAR) Star (the type of types) is now a term
  |  Pi CTerm CTerm --(PI) dependent function space
  |  Bound  Int --Bound variables of type Int because it's represented by de Bruijn indices
  |  Free   Name --Free variables of type name (see below)
  |  ITerm :@: CTerm --the infix constructor :@: denotes application
  |  Nat --Nat data type
  |  NatElim CTerm CTerm CTerm CTerm --eliminator of Nat
  |  Zero --data constructor of Nat
  |  Succ CTerm --data constructor of Nat
  |  Vec CTerm CTerm --Vec (vector) data type
  |  Nil CTerm --data constructor of Vec
  |  Cons CTerm CTerm CTerm CTerm --data constructor of Vec
  |  VecElim CTerm CTerm CTerm CTerm CTerm CTerm --elminator of Vec
  deriving (Show, Eq)
  
--Checkable terms
data CTerm
  =  Inf  ITerm --(CHK) Inf is the constructor that embeds ITerm to CTerm 
  |  Lam  CTerm --(LAM) Lam stands for Lambda abstractions
  deriving (Show, Eq)

data Name
  =  Global  String --Global variables are represented by name thus type string
  |  Local   Int --to convert a bound variable into a free one
  |  Quote   Int 
  deriving (Show, Eq)

--Values can now also be VStar or VPi
data Value
  = VLam (Value -> Value)
  | VStar
  | VPi Value (Value -> Value)
  | VNeutral Neutral
  | VNat --extensions for Nat
  | VZero
  | VSucc Value
  | VNil Value --extensions for Vec
  | VCons Value Value Value Value
  | VVec Value Value

--A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value
  | NNatElim Value Value Value Neutral --for NatElim to not be stuck
  | NVecElim Value Value Value Value Value Neutral

--vfree creates the value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

--Contexts map variables to their types.
type Type    = Value 
type Context = [(Name, Type)]

--Evaluation, including Natural numbers
type Env = [Value]
iEval :: ITerm -> Env -> Value
iEval (Ann  e _)    d  =  cEval e d
iEval (Free  x)     d  =  vfree x
iEval (Bound  ii)   d  =  d !! ii --(!!) :: [a] -> Int -> a. It's the list lookup operator. 
iEval (e1 :@: e2)   d  =  vapp (iEval e1 d) (cEval e2 d)
iEval Star          d  =  VStar
iEval (Pi ty ty')   d  =  VPi (cEval ty d)(\ x -> cEval ty' (x : d))
--evaluation of Nat
iEval Nat                  d  =  VNat 
iEval Zero                 d  =  VZero
iEval (Succ k)             d  =  VSucc (cEval k d)
iEval (NatElim m mz ms k)  d
  = let mzVal = cEval mz d
        msVal = cEval ms d
        rec kVal =
          case kVal of
            VZero      -> mzVal
            VSucc l    -> msVal `vapp` l `vapp` rec l
            VNeutral k -> VNeutral
                          (NNatElim (cEval m d) mzVal msVal k)
            _          -> error "internal: eval natElim"
    in rec (cEval k d)
--evaluation of Vec (TODO)
iEval_ (Vec_ a n)                 d  =  VVec_ (cEval_ a d) (cEval_ n d)
iEval_ (VecElim_ a m mn mc n xs)  d  =
    let  mnVal  =  cEval_ mn d
         mcVal  =  cEval_ mc d
         rec nVal xsVal =
           case xsVal of
             VNil_ _          ->  mnVal
             VCons_ _ k x xs  ->  foldl vapp_ mcVal [k, x, xs, rec k xs]
             VNeutral_ n      ->  VNeutral_
                                  (NVecElim_  (cEval_ a d) (cEval_ m d)
                                              mnVal mcVal nVal n)
             _                ->  error "internal: eval vecElim"
    in   rec (cEval_ n d) (cEval_ xs d)

vapp :: Value -> Value -> Value
vapp (VLam f)      v  =  f v
vapp (VNeutral n)  v  =  VNeutral (NApp n v)

cEval :: CTerm -> Env -> Value
cEval (Inf  ii)   d  =  iEval ii d
cEval (Lam  e)    d  =  VLam (\ x -> cEval e (x : d))
--for Nat (in the paper, they are iEval??? But in the code, they are cEval???)
cEval Zero      d  = VZero
cEval (Succ k)  d  = VSucc (cEval k d)
--for Vec (not mentioned in the paper, only in the code???)
cEval (Nil a)          d  =  VNil (cEval a d)
cEval (Cons a n x xs)  d  =  VCons  (cEval a d) (cEval n d)
                                    (cEval x d) (cEval xs d)

--Substitution

--substitution function for inferable terms
iSubst :: Int -> ITerm -> ITerm -> ITerm
iSubst ii r (Ann e ty)     =  Ann (cSubst ii r e) (cSubst ii r ty)
iSubst ii r (Bound j)      =  if ii == j then r else Bound j
iSubst ii r (Free y)       =  Free y
iSubst ii r (e1 :@: e2)    =  iSubst ii r e1 :@: cSubst ii r e2
iSubst ii r Star           =  Star
iSubst ii r (Pi ty ty')    =  Pi  (cSubst ii r ty) (cSubst (ii + 1) r ty')
--for Nat
iSubst ii r Nat            =  Nat
iSubst_ ii r  (NatElim m mz ms n)
                           =  NatElim (cSubst ii r m)
                                      (cSubst ii r mz) (cSubst ii r ms)
                                      (cSubst ii r ms)

--substitution function for checkable terms
cSubst :: Int -> ITerm -> CTerm -> CTerm
cSubst ii r (Inf e)      =  Inf (iSubst ii r e)
cSubst ii r (Lam e)      =  Lam (cSubst (ii + 1) r e)

--Quotation: takes a value back to a term
quote0 :: Value -> CTerm
quote0 = quote 0

quote :: Int -> Value -> CTerm
quote ii (VLam f)      =  Lam (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n)  =  Inf (neutralQuote ii n)
quote ii VStar         = Inf Star
quote ii (VPi v f)     = Inf (Pi (quote ii v)(quote (ii + 1)(f (vfree(Quote ii)))))
--for Nat
quote ii VNat       =  Inf Nat
quote ii VZero      =  Inf Zero
quote ii (VSucc n)  =  Inf (Succ (quote ii n))

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote ii (NFree x)   =  boundfree ii x
neutralQuote ii (NApp n v)  =  neutralQuote ii n :@: quote ii v

--checks if the variable occurring at the head of the application is a bound variable or a free name
boundfree :: Int -> Name -> ITerm
boundfree ii (Quote k)     =  Bound (ii - k - 1)
boundfree ii x             =  Free x

--Type checking

type Result a = Either String a --when type checking fails, it throws an error.

--inferable terms has type as output.
iType0 :: Context -> ITerm -> Result Type
iType0 = iType 0

iType :: Int -> Context -> ITerm -> Result Type
iType ii g (Ann e rho)
  =  do cType ii g rho VStar
        let ty = cEval rho []
        cType ii g e ty
        return ty
iType ii g Star
  =  return VStar
iType ii g (Pi rho rho')
  = do cType ii g rho VStar
       let ty = cEval rho []
       cType (ii + 1) ((Local ii, ty): g)
             (cSubst 0 (Free (Local ii))rho') VStar
       return VStar
iType ii g (Free x)
  =  case lookup x g of
        Just ty  ->  return ty
        Nothing  ->  throwError "unknown identifier"
iType ii g (e1 :@: e2)
  = do si <- iType ii g e1
       case si of
         VPi ty ty'  ->  do  cType ii g e2 ty
                             return (ty' (cEval e2 []))
         _           ->  throwError "illegal application"
--type checker for Nat
iType ii g Nat                  =  return VStar
iType ii g Zero                 =  return VNat
iType ii g (Succ k)             = 
  do cType ii g k VNat
     return VNat
iType ii g (NatElim m mz ms k)  =
  do cType ii g m (VPi VNat (const VStar))
     let mVal = cEval m []
     cType ii g mz (mVal `vapp` VZero)
     cType ii g ms (VPi VNat (\ l -> VPi (mVal `vapp` l) (\_ -> mVal `vapp` VSucc l)))
     cType ii g k VNat
     let kVal = cEval k []
     return (mVal `vapp` kVal)

--checkable terms takes a type as input and returns ().
cType :: Int -> Context -> CTerm -> Type -> Result ()
cType ii g (Inf e) v
  =  do v' <- iType ii g e
        unless (quote0 v == quote0 v') (throwError "type mismatch") --throwError only when ty ==ty' is false.
cType ii g (Lam e) (VPi ty ty')
  =  cType (ii + 1) ((Local ii, ty) : g)
           (cSubst 0 (Free (Local ii)) e) (ty' (vfree (Local ii)))
cType ii g _ _
  =  throwError "type mismatch" 


