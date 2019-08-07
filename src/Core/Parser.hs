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
                , Token.reservedNames   = [ "*","Nat","Zero" --ITerms without inputs
                                          ]
                , Token.reservedOpNames = [ "Inf", "Lam"]
                }

  lexer = Token.makeTokenParser languageDef

  identifier = Token.identifier lexer -- parses an identifier
  reserved   = Token.reserved   lexer -- parses a reserved name
  reservedOp = Token.reservedOp lexer -- parses an operator
  parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                      --   parens p
                                      -- takes care of the parenthesis and
                                      -- uses p to parse what's inside them
  integer    = Token.integer    lexer -- parses an integer
  whiteSpace = Token.whiteSpace lexer -- parses whitespace

  whileParser :: Parser ITerm
  whileParser = whiteSpace >> term

  reservedAs (str,term) = reserved str >> return term

  reservedSimple = [("*", Star), ("Nat", Nat), ("Zero", Zero)]

  term :: Parser ITerm
  term =  parens term
      <|> foldr1 (<|>) (map reservedAs reservedSimple) --ITerms without inputs
      
              
  cterm :: Parser CTerm
  cterm =  parens cterm
       <|> do reservedOp "Inf"
              iterm <- term
              return $ Inf iterm

  parseString :: String -> ITerm
  parseString str =
    case parse term "" str of
      Left e -> error $ show e
      Right r -> r

  parseStringC :: String -> CTerm
  parseStringC str =
    case parse cterm "" str of
      Left e -> error $ show e
      Right r -> r


