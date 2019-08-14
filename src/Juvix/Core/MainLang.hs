-- Quantitative type implementation following
-- Atkey 2017 and McBride 2016.
module Juvix.Core.MainLang where

  import           Prelude
  import           Control.Monad.Except
  import           Numeric.Natural

  -- Inferable terms
  data ITerm
    =  Star Natural             -- (sort i) i th ordering of (closed) universe.
    |  Natural                  -- (Prim) primitive type
    |  Pi Natural CTerm CTerm   -- (PI) dependent function space, the first input (n:Natural)  
                                -- tracks how many times the second input (x:CTerm) is used.
    |  Pm Natural CTerm CTerm   -- dependent multiplicative conjunction (tensor product)
    |  Pa Natural CTerm CTerm   -- dependent additive conjunction type
    |  NPm CTerm CTerm      -- non-dependent multiplicative disjunction type
    |  Bound  Natural       -- Bound variables of type Natural because it's represented by de Bruijn indices
    |  Free   Name          -- Free variables of type name (see below)
    |  ITerm :@: CTerm      -- the infix constructor :@: denotes application
    deriving (Show, Eq)

  --Checkable terms
  data CTerm
    =  Inf  ITerm --(CHK) Inf is the constructor that embeds ITerm to CTerm
    |  Lam  CTerm --(LAM) Lam stands for Lambda abstractions
    deriving (Show, Eq)

  data Name
    =  Global  String -- Global variables are represented by name thus type string
    |  Local   Natural    -- to convert a bound variable into a free one
    |  Quote   Natural
    deriving (Show, Eq)
