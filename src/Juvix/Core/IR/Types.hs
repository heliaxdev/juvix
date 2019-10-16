module Juvix.Core.IR.Types where

import Control.Lens ((^?), ix)
import Control.Monad.Except (throwError)
import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (Show (..), String, error, lookup)

-- Quantitative type implementation inspired by
-- Atkey 2017 and McBride 2016.

-- | checkable terms
data CTerm
  = -- | (sort i) i th ordering of (closed) universe.
    Star Natural
  | -- | 'Prim' primitive type (naturals)
    Nats
  | -- | formation rule of the dependent function type 'PI'.
    -- the Usage(π) tracks how many times x is used.
    Pi Usage CTerm CTerm
  | -- | 'LAM' Introduction rule of PI.
    -- The abstracted variable's usage is tracked with the Usage(π).
    Lam CTerm
  | -- | 'CONV' conversion rule. TODO make sure 0Γ ⊢ S≡T
    -- 'Conv' is the constructor that embeds ITerm to CTerm
    Conv ITerm
  deriving (Eq)

instance Show CTerm where
  show (Star n) = "* " <> show n
  show Nats = "Nat "
  show (Pi _usage varTy resultTy) =
    "[Π] " <> show varTy <> "-> " <> show resultTy
  show (Lam var) = "\\x. " <> show var
  -- Conv should be invisible to users.
  show (Conv term) = show term

-- | inferable terms
data ITerm
  = -- | Bound variables, in de Bruijn indices
    Bound Natural
  | -- | Free variables of type name (see below)
    Free Name
  | -- | primitive constant (naturals)
    Nat Natural
  | -- | elimination rule of PI (APP).
    App ITerm CTerm
  | -- | Annotation with usage.
    Ann Usage CTerm CTerm
  deriving (Eq)

instance Show ITerm where
  show (Bound i) = "Bound " <> show i -- to be improved
  show (Free name) = show name -- using derived show Name instance, to be improved
  show (Nat i) = show i
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
data Value
  = VStar Natural
  | VNats
  | VPi Usage Value (Value → Value)
  | VLam (Value → Value)
  | VNeutral Neutral
  | VNat Natural

-- | A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value
  deriving (Show, Eq)

-- | 'Annotations' include usage and type.
type Annotation = (Usage, Value)

-- Contexts map variables to their types.
type Context = [(Name, Annotation)]

-- Evaluation
type Env = [Value]

instance Eq Value where
  x == y = quote0 x == quote0 y

instance Show Value where
  show x = show (quote0 x)

-- Quotation: takes a value back to a term
quote0 ∷ Value → CTerm
quote0 = quote 0

quote ∷ Natural → Value → CTerm
quote _ii (VStar n) = Star n
quote _ii VNats = Nats
quote ii (VPi pi v f) =
  Pi pi (quote ii v) (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VLam f) = Lam (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n) = Conv (neutralQuote ii n)
quote _ii (VNat n) = Conv (Nat n)

neutralQuote ∷ Natural → Neutral → ITerm
neutralQuote ii (NFree x) = boundfree ii x
neutralQuote ii (NApp n v) = App (neutralQuote ii n) (quote ii v)

-- | 'vfree' creates the value corresponding to a free variable
vfree ∷ Name → Value
vfree n = VNeutral (NFree n)

-- checks if the variable occurring at the head of the application is a bound variable or a free name
boundfree ∷ Natural → Name → ITerm
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree _ii x = Free x
