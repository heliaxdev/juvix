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
                , Token.reservedNames   = [ "*","Nat","Zero", --ITerms without inputs
                                            "Ann"
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

  --Enable parsing of white space.
  parseWS :: Parser a -> Parser a
  parseWS p = whiteSpace >> p
  
  parseSimpleI (str,term) = reserved str >> return term
  --List of simple ITerms without inputs
  reservedSimple = [("*", Star), ("Nat", Nat), ("Zero", Zero)]
   
  annTerm :: Parser ITerm
  annTerm = 
    do reserved "Ann"
       c1 <- cterm
       c2 <- cterm
       return $ Ann c1 c2

  term :: Parser ITerm
  term =  parens term
      <|> foldr1 (<|>) (map parseSimpleI reservedSimple) --ITerms without inputs
      <|> annTerm
              
  cterm :: Parser CTerm
  cterm =  parens cterm
       <|> do reservedOp "Inf"
              iterm <- term
              return $ Inf iterm
       <|> do reservedOp "Lam"
              cTerm <- cterm
              return $ Lam cTerm

  parseString :: Parser a -> String -> a
  parseString p str =
    case parse (parseWS p) "" str of
      Left e -> error $ show e
      Right r -> r


