module Juvix.Core.Types where

import Juvix.Library
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

data Parameterisation primTy primVal
  = Parameterisation
      { typeOf ∷ primVal → primTy,
        apply ∷ primVal → primVal → Maybe primVal,
        parseTy ∷ Token.GenTokenParser String () Identity → Parser primTy,
        parseVal ∷ Token.GenTokenParser String () Identity → Parser primVal
      }
