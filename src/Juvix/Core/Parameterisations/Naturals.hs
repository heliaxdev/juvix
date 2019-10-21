module Juvix.Core.Parameterisations.Naturals where

import Juvix.Core.Types hiding (apply, parseTy, parseVal, typeOf)
import Juvix.Library
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

data NatTy
  = Nat
  deriving (Show, Eq)

data NatVal
  = Natural Natural
  deriving (Show, Eq)

typeOf ∷ NatVal → NatTy
typeOf (Natural _) = Nat

apply ∷ NatVal → NatVal → Maybe NatVal
apply _ _ = Nothing

parseTy ∷ Token.GenTokenParser String () Identity → Parser NatTy
parseTy lexer = do
  Token.reserved lexer "Nat"
  pure Nat

parseVal ∷ Token.GenTokenParser String () Identity → Parser NatVal
parseVal lexer = Natural . fromIntegral |<< Token.natural lexer

naturals ∷ Parameterisation NatTy NatVal
naturals = Parameterisation typeOf apply parseTy parseVal
