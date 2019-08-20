module Juvix.Core.Parser where

import           Prelude
import           Juvix.Core.MainLang
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import           Data.Functor.Identity
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef ∷ GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
     emptyDef { Token.commentStart    = "/*"
              , Token.commentEnd      = "*/"
              , Token.commentLine     = "//"
              , Token.identStart      = letter
              , Token.identLetter     = alphaNum
              , Token.reservedNames   = [ "Pi", "App" --ITerms with CTerms as inputs
                                          "Bound", --Bound var
                                          "Free","Local","Quote","Global" --Free var
                                        ]
              , Token.reservedOpNames = [ "Inf", "Lam", ":",
                                          ":@:" --application
                                        ]
              }
lexer ∷ Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

-- These are parsers for what their names signify
identifier ∷ Parser String
identifier = Token.identifier lexer

reserved ∷ String → Parser ()
reserved   = Token.reserved lexer

reservedOp ∷ String → Parser ()
reservedOp = Token.reservedOp lexer

parens ∷ Parser String → Parser String
parens = Token.parens lexer -- parses surrounding parenthesis, and what is inside them

natural ∷ Parser Integer
natural = Token.natural lexer

whiteSpace ∷ Parser ()
whiteSpace = Token.whiteSpace lexer

parseSimpleI ∷ (String, b) → Parser b
parseSimpleI (str,term) = reserved str >> return term

--Parser for naturals.
nats ∷ Parser Integer
nats =  parens nats
      <|> natural

piTerm ∷ Parser ITerm
piTerm =
  do reserved "Pi"
     pi <- nats
     input <- cterm
     func <- cterm
     return $ Pi input func

boundTerm ∷ Parser ITerm
boundTerm =
  do reserved "Bound"
     index <- nats
     return $ Bound (fromInteger index)

--Parser for the global free variable name
gName ∷ Parser String
gName =  parens gName
     <|> identifier
--Parser for Name data type
name ∷ Parser Name
name =  do reserved "Local"
           index <- nats
           return $ Local (fromInteger index)
    <|> do reserved "Quote"
           index <- nats
           return $ Quote (fromInteger index)
    <|> do reserved "Global"
           gname <- gName
           return $ Global gname

freeTerm ∷ Parser ITerm
freeTerm =
  do reserved "Free"
     fname <- name
     return $ Free fname

appTerm ∷ Parser ITerm
appTerm =
  do reserved "App"
     pi <- nats
     iterm <- term
     cTerm <- cterm
     eof
     return $ App pi term cterm

iterm ∷ Parser ITerm
iterm =  appTerm --Application
     <|> term --all ITerms except the application ITerm

parseWhole ∷ Parser ITerm
parseWhole =
  do whiteSpace
     t <- iterm
     whiteSpace
     eof
     return t

term ∷ Parser ITerm
term =  parens term
    <|> appTerm
    --ITerms with CTerms as input(s).
    <|> piTerm
    <|> boundTerm --Bound var
    <|> freeTerm --Free var

cterm ∷ Parser CTerm
cterm =  parens cterm
     <|> do reservedOp "Inf"
            iterm <- term
            return $ Inf iterm
     <|> do reservedOp "Lam"
            pi <- nats
            cTerm <- cterm
            return $ Lam pi cTerm

parseString ∷ Parser a → String → a
parseString p str =
  case parse p "" str of
       Left e → error $ show e
       Right r → r
