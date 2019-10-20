module Juvix.Core.HR.Parser where

import Data.Functor.Identity
import Juvix.Core.HR.Types
import Juvix.Core.Usage
import Juvix.Library hiding ((<|>))
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language hiding (reservedNames, reservedOpNames)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

type STerm = Term () ()

type SElim = Elim () ()

languageDef ∷ GenLanguageDef String u Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames = reservedNames,
      Token.reservedOpNames = reservedOpNames
    }

reservedNames ∷ [String]
reservedNames =
  [ "*", -- sort
    "[Π]", -- function type
    "w" -- omega
  ]

reservedOpNames ∷ [String]
reservedOpNames =
  [ "\\", -- lambda
    "@", -- TODO: remove me, necessary for annotation parsing at the moment
    ":", -- type & usage annotation
    "->" -- arrow
  ]

ops ∷ [[Operator Char () SElim]]
ops = [[Infix appl AssocLeft]]

appl ∷ Parser (SElim → SElim → SElim)
appl = do
  whiteSpace
  notFollowedBy (choice (map reservedOp reservedOpNames))
  pure (\f x → App f (Elim x))

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

binder ∷ Parser Symbol
binder = intern |<< identifier

term ∷ Parser STerm
term = termOnly <|> elimTerm

termOnly ∷ Parser STerm
termOnly =
  parens termOnly <|> sortTerm <|> piTerm <|> lamTerm

elimTerm ∷ Parser STerm
elimTerm = do
  elim ← elim
  pure (Elim elim)

annElim ∷ Parser SElim
annElim = do
  reservedOp "@"
  theTerm ← termOnly
  reservedOp ":"
  pi ← usage
  theType ← term
  eof
  pure (Ann pi theTerm theType)

varElim ∷ Parser SElim
varElim = Var |<< binder

elim ∷ Parser SElim
elim = buildExpressionParser ops elim'

elim' ∷ Parser SElim
elim' =
  parens elim <|> varElim <|> annElim

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
