{-# LANGUAGE ExplicitForAll #-}

module Juvix.Core.Parameterisations.Unit where

import           Juvix.Core.Types                    hiding (apply, parseTy,
                                                      parseVal, reservedNames,
                                                      reservedOpNames, typeOf)
import           Juvix.Library                       hiding ((<|>))
import           Prelude                             (String)
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token

-- k: primitive type: unit
data UnitTy =
  TUnit
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data UnitVal =
  Unit
  deriving (Show, Eq)

typeOf :: forall a. ([UnitTy] -> a) -> UnitVal -> Either a UnitTy
typeOf _ Unit = Right TUnit

apply :: UnitVal -> UnitVal -> Maybe UnitVal
apply _ _ = Nothing

parseTy :: Token.GenTokenParser String () Identity -> Parser UnitTy
parseTy lexer = do
  Token.reserved lexer "TUnit"
  pure TUnit

parseVal :: Token.GenTokenParser String () Identity -> Parser UnitVal
parseVal lexer = do
  Token.reserved lexer "Unit"
  pure Unit

reservedNames :: [String]
reservedNames = ["TUnit", "Unit"]

reservedOpNames :: [String]
reservedOpNames = []

unit :: Parameterisation UnitTy UnitVal
unit =
  Parameterisation typeOf apply parseTy parseVal reservedNames reservedOpNames
