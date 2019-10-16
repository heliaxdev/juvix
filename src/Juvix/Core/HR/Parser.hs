module Juvix.Core.HR.Parser where

import Data.Functor.Identity
import qualified Data.Text as Text
import Juvix.Core.HR.Types
import Juvix.Core.Usage
import Juvix.Library hiding ((<|>))
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

type STerm = Term () () Text

type SElim = Elim () () Text

languageDef ∷ GenLanguageDef String u Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames =
        [ "*", -- sort
          "[Π]", -- function type
          "w" -- omega
        ],
      Token.reservedOpNames =
        [ "\\", -- lambda
          ":", -- type & usage annotation
          "->" -- arrow
        ]
    }

lexer ∷ Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

-- These are parsers for what their names signify

identifier ∷ Parser String
identifier = Token.identifier lexer

reserved ∷ String → Parser ()
reserved = Token.reserved lexer

reservedOp ∷ String → Parser ()
reservedOp = Token.reservedOp lexer

parens ∷ Parser a → Parser a
parens = Token.parens lexer -- parses surrounding parentheses, and what is inside them

natural ∷ Parser Integer
natural = Token.natural lexer

whiteSpace ∷ Parser ()
whiteSpace = Token.whiteSpace lexer

usage ∷ Parser Usage
usage =
  do
    reserved "w"
    return Omega
    <|> do SNat . fromInteger <$> natural

sortTerm ∷ Parser STerm
sortTerm = do
  reserved "*"
  n ← natural
  return $ Star (fromInteger n)

piTerm ∷ Parser STerm
piTerm = do
  reserved "[Π]"
  pi ← usage
  input ← term
  func ← term
  return $ Pi pi input func

lamTerm ∷ Parser STerm
lamTerm = do
  reservedOp "\\"
  binder ← binder
  reservedOp "->"
  func ← term
  return $ Lam binder func

binder ∷ Parser Text
binder = Text.pack |<< identifier

convTerm ∷ Parser STerm
convTerm = do
  elim ← elim
  pure (Elim elim)

appElim ∷ Parser SElim
appElim = do
  func ← term
  whiteSpace
  val ← elim
  eof
  return $ App func val

annElim ∷ Parser SElim
annElim = do
  theTerm ← term
  reservedOp ":"
  pi ← usage
  theType ← term
  eof
  return $ Ann pi theTerm theType

varElim ∷ Parser SElim
varElim = Var |<< binder

term ∷ Parser STerm
term =
  parens term <|> sortTerm <|> piTerm <|> lamTerm <|> convTerm

elim ∷ Parser SElim
elim =
  parens elim <|> varElim <|> appElim <|> annElim

parseWhole ∷ Parser a → Parser a
parseWhole p = do
  whiteSpace
  t ← p
  whiteSpace
  eof
  return t

parseString ∷ Parser a → String → Maybe a
parseString p str =
  case parse p "" str of
    Left _ → Nothing
    Right r → Just r
{-
 - TODO

natTypeTerm ∷ Parser STerm
natTypeTerm = do
  reserved "Nat"
  return Nats

natTerm ∷ Parser ITerm
natTerm = Nat . fromInteger <$> natural

natAddTerm ∷ Parser Value
natAddTerm = do
  reservedOp "+"
  x <- natural
  y <- natural
  eof
  return $ natOp (+) (Nat (fromInteger x)) (Nat (fromInteger y))

natSubTerm ∷ Parser Value
natSubTerm = do
  reservedOp "-"
  x <- natural
  y <- natural
  eof
  return $ natOp (-) (Nat (fromInteger x)) (Nat (fromInteger y))

natMultTerm ∷ Parser Value
natMultTerm = do
  reservedOp "*"
  x <- natural
  y <- natural
  eof
  return $ natOp (*) (Nat (fromInteger x)) (Nat (fromInteger y))

--parser for values
pValue :: Parser Value
pValue = parens pValue <|> natAddTerm <|> natSubTerm <|> natMultTerm
-}
