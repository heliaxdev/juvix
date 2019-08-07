module Parser where

  import MainLang

  import Text.Parsec
  import System.IO
  import Control.Monad
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  --Takes a string and output an ITerm.
  --iTerms :: Parser ITerms

  languageDef =
       emptyDef { Token.commentStart    = "/*"
                , Token.commentEnd      = "*/"
                , Token.commentLine     = "//"
                , Token.identStart      = letter
                , Token.identLetter     = alphaNum
                , Token.reservedNames   = [ "*"
                                          ]
                }

  lexer = Token.makeTokenParser languageDef

  identifier = Token.identifier lexer -- parses an identifier
  reserved   = Token.reserved   lexer -- parses a reserved name
  parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                      --   parens p
                                      -- takes care of the parenthesis and
                                      -- uses p to parse what's inside them
  integer    = Token.integer    lexer -- parses an integer
  whiteSpace = Token.whiteSpace lexer -- parses whitespace

  whileParser :: Parser ITerm
  whileParser = whiteSpace >> term

  term :: Parser ITerm
  term = parens term
      <|> star

  star :: Parser ITerm
  star =
    do reserved "*"
       return Star

  parseString :: String -> ITerm
  parseString str =
    case parse term "" str of
      Left e -> error $ show e
      Right r -> r


