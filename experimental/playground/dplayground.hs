--Dependent type implementation following 
--"A tutorial implementation of a dependently typed lambda calculus"

import Control.Monad.Except --Enable throwError 
import System.Console.Haskeline
import System.IO hiding (print)
 
-- Inferable terms 
data ITerm 
  =  Ann    CTerm Type --Annotated terms
  |  Bound  Int --Bound variables
  |  Free   Name --Free variables
  |  ITerm :@: CTerm --The infix constructor :@: denotes application
  deriving (Show, Eq)
  
--Checkable terms
data CTerm
  =  Inf  ITerm --Inf is the constructor that embeds ITerm to CTerm 
  |  Lam  CTerm --Lam stands for Lambda abstractions
  deriving (Show, Eq)

data Name
  =  Global  String 
  |  Local   Int --to convert a bound variable into a free one
  |  Quote   Int 
  deriving (Show, Eq)

--Type is either base type or function type
data Type
  = TFree Name
  | Fun Type Type
  deriving (Show, Eq)
    
--Values are lambda abstractions or neutral terms
data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

--A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value

--vfree creates the value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

--Evaluation

type Env = [Value]
type NameEnv v = [(Name, v)] 
iEval :: ITerm -> (NameEnv Value,Env) -> Value
iEval (Ann  e _)    d  =  cEval e d
iEval (Free  x)     d  =  case lookup x (fst d) of Nothing ->  (vfree x); Just v -> v
iEval (Bound  ii)   d  =  (snd d) !! ii
iEval (e1 :@: e2)   d  =  vapp (iEval e1 d) (cEval e2 d)

vapp :: Value -> Value -> Value
vapp (VLam f)      v  =  f v
vapp (VNeutral n)  v  =  VNeutral (NApp n v)

cEval :: CTerm -> (NameEnv Value,Env) -> Value
cEval (Inf  ii)   d  =  iEval ii d
cEval (Lam  e)    d  =  VLam (\ x -> cEval e (((\(e, d) -> (e,  (x : d))) d)))

--Contexts

data Kind = Star --base type 
  deriving (Show)

data Info --A name is either of a base type or of a type
  = HasKind Kind
  | HasType Type
  deriving (Show)

--A context is a list of pairs of a name and its info.
--Extend a context with the list cons operation.
--Look up a name in a context with the list lookup function.
type Context = [(Name, Info)]

type Result a = Either String a

--Type checking
cKind :: Context -> Type -> Kind -> Result ()
cKind g (TFree x) Star
  =  case lookup x g of
        Just (HasKind Star)  ->  return ()
        Nothing              ->  throwError "unknown identifier"
cKind g (Fun kk kk') Star
  =  do cKind g kk Star
        cKind g kk' Star

iType0 :: Context -> ITerm -> Result Type
iType0 = iType 0

iType :: Int -> Context -> ITerm -> Result Type
iType ii g (Ann e ty)
  =  do cKind g ty Star
        cType ii g e ty
        return ty
iType ii g (Free x)
  =  case lookup x g of
        Just (HasType ty)  ->  return ty
        Nothing            ->  throwError "unknown identifier"
iType ii g (e1 :@: e2)
  = do si <- iType ii g e1
       case si of
         Fun ty ty'  ->  do  cType ii g e2 ty
                             return ty'
         _           ->  throwError "illegal application"

cType :: Int -> Context -> CTerm -> Type -> Result ()
cType ii g (Inf e) ty
  =  do ty' <- iType ii g e
        unless (ty == ty') (throwError "type mismatch")
cType ii g (Lam e) (Fun ty ty')
  =  cType  (ii + 1) ((Local ii, HasType ty) : g)
            (cSubst 0 (Free (Local ii)) e) ty'
cType ii g _ _
  =  throwError "type mismatch" 

--Substitution
iSubst :: Int -> ITerm -> ITerm -> ITerm
iSubst ii r (Ann e ty)   =  Ann (cSubst ii r e) ty
iSubst ii r (Bound j)    =  if ii == j then r else Bound j
iSubst ii r (Free y)     =  Free y
iSubst ii r (e1 :@: e2)  =  iSubst ii r e1 :@: cSubst ii r e2

cSubst :: Int -> ITerm -> CTerm -> CTerm
cSubst ii r (Inf e)      =  Inf (iSubst ii r e)
cSubst ii r (Lam e)      =  Lam (cSubst (ii + 1) r e)

--Quotation: takes a value back to a term
quote0 :: Value -> CTerm
quote0 = quote 0

quote :: Int -> Value -> CTerm
quote ii (VLam f)      =  Lam (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n)  =  Inf (neutralQuote ii n)

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote ii (NFree x)   =  boundfree ii x
neutralQuote ii (NApp n v)  =  neutralQuote ii n :@: quote ii v

--checks if the variable occurring at the head of the application is a bound variable or a free name
boundfree :: Int -> Name -> ITerm
boundfree ii (Quote k)     =  Bound (ii - k - 1)
boundfree ii x             =  Free x

--definitions for testing
id'      =  Lam (Inf (Bound 0))
const'   =  Lam (Lam (Inf (Bound 1)))

tfree a  =  TFree (Global a)
free x   =  Inf (Free (Global x))

term1    =  Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y" 
term2    =  Ann const' (Fun  (Fun (tfree "b") (tfree "b"))
                              (Fun  (tfree "a")
                                    (Fun (tfree "b") (tfree "b"))))
            :@: id' :@: free "y" 

env1     =  [  (Global "y", HasType (tfree "a")),
                (Global "a", HasKind Star)]
env2     =  [(Global "b", HasKind Star)] ++ env1
