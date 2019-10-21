module Juvix.Core.IR.Types where

import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (Show (..), String)

-- Quantitative type implementation inspired by
-- Atkey 2017 and McBride 2016.

-- | checkable terms
data Term primTy primVal
  = -- | (sort i) i th ordering of (closed) universe.
    Star Natural
  | -- | 'PrimTy' primitive type
    PrimTy primTy
  | -- | formation rule of the dependent function type 'PI'.
    -- the Usage(π) tracks how many times x is used.
    Pi Usage (Term primTy primVal) (Term primTy primVal)
  | -- | 'LAM' Introduction rule of PI.
    -- The abstracted variable's usage is tracked with the Usage(π).
    Lam (Term primTy primVal)
  | -- | 'CONV' conversion rule. TODO make sure 0Γ ⊢ S≡T
    -- 'Elim' is the constructor that embeds Elim to Term
    Elim (Elim primTy primVal)
  deriving (Eq)

instance (Show primTy, Show primVal) ⇒ Show (Term primTy primVal) where
  show (Star n) = "* " <> show n
  show (PrimTy p) = show p
  show (Pi _usage varTy resultTy) =
    "[Π] " <> show varTy <> "-> " <> show resultTy
  show (Lam var) = "\\x. " <> show var
  -- Elim should be invisible to users.
  show (Elim term) = show term

-- | inferable terms
data Elim primTy primVal
  = -- | Bound variables, in de Bruijn indices
    Bound Natural
  | -- | Free variables of type name (see below)
    Free Name
  | -- | primitive constant
    Prim primVal
  | -- | elimination rule of PI (APP).
    App (Elim primTy primVal) (Term primTy primVal)
  | -- | Annotation with usage.
    Ann Usage (Term primTy primVal) (Term primTy primVal)
  deriving (Eq)

instance (Show primTy, Show primVal) ⇒ Show (Elim primVal primTy) where
  show (Bound i) = "Bound " <> show i -- to be improved
  show (Free name) = show name -- using derived show Name instance, to be improved
  show (Prim p) = show p
  show (App f x) = show f <> show x
  show (Ann pi theTerm theType) =
    show theTerm <> " : [" <> show pi <> "] " <> show theType

data Name
  = -- | Global variables are represented by name thus type string
    Global String
  | -- | to convert a bound variable into a free one
    Local Natural
  | Quote Natural
  deriving (Show, Eq)

-- | Values/types
data Value primTy primVal
  = VStar Natural
  | VPrimTy primTy
  | VPi Usage (Value primTy primVal) (Value primTy primVal → Value primTy primVal)
  | VLam (Value primTy primVal → Value primTy primVal)
  | VNeutral (Neutral primTy primVal)
  | VPrim primVal

-- | A neutral term is either a variable or an application of a neutral term to a value
data Neutral primTy primVal
  = NFree Name
  | NApp (Neutral primTy primVal) (Value primTy primVal)
  deriving (Show, Eq)

-- | 'Annotations' include usage and type.
type Annotation primTy primVal = (Usage, Value primTy primVal)

-- Contexts map variables to their types.
type Context primTy primVal = [(Name, Annotation primTy primVal)]

-- Evaluation
type Env primTy primVal = [Value primTy primVal]

instance (Eq primTy, Eq primVal) ⇒ Eq (Value primTy primVal) where
  x == y = quote0 x == quote0 y

instance (Show primTy, Show primVal) ⇒ Show (Value primTy primVal) where
  show x = show (quote0 x)

-- Quotation: takes a value back to a term
quote0 ∷ Value primTy primVal → Term primTy primVal
quote0 = quote 0

quote ∷ Natural → Value primTy primVal → Term primTy primVal
quote _ii (VStar n) = Star n
quote _ii (VPrimTy p) = PrimTy p
quote ii (VPi pi v f) =
  Pi pi (quote ii v) (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VLam f) = Lam (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n) = Elim (neutralQuote ii n)
quote _ii (VPrim p) = Elim (Prim p)

neutralQuote ∷ Natural → Neutral primTy primVal → Elim primTy primVal
neutralQuote ii (NFree x) = boundfree ii x
neutralQuote ii (NApp n v) = App (neutralQuote ii n) (quote ii v)

-- | 'vfree' creates the value corresponding to a free variable
vfree ∷ Name → Value primTy primVal
vfree n = VNeutral (NFree n)

-- checks if the variable occurring at the head of the application is a bound variable or a free name
boundfree ∷ Natural → Name → Elim primTy primVal
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree _ii x = Free x
