module Juvix.Core.Parser where

import           Juvix.Core.MainLang     
import           Data.Functor.Identity
import           Prelude
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames =
        [ "*" --sort
        , "[Π]" --function type
        , "[π]" --dependent multiplicative conjunction type
        , "/\\" --dependent additive conjunction type
        , "\\/" --non-dependent multiplicative disjunction type
        , "Bound" --Bound var
        , "Free"
        , "Local"
        , "Quote"
        , "Global" --Free var
        , "w" --Omega
        ]
    , Token.reservedOpNames = ["Inf", "\\", ":"]
    }

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef
     -- These are parsers for what their names signify

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer -- parses surrounding parenthesis, and what is inside them

natural :: Parser Integer
natural = Token.natural lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
     
natw ∷ Parser NatAndw
natw =   do reserved "w"
            return Omega
     <|> do n <- natural
            return $ Natural (fromInteger n) 
            
sortTerm :: Parser ITerm
sortTerm =
     do reserved "*"
        n <- natural
        return $ Star (fromInteger n)

piTerm ∷ Parser ITerm
piTerm = 
     do reserved "[Π]"
        pi <- natw
        input <- cterm
        func <- cterm
        return $ Pi pi input func
        
pmTerm ∷ Parser ITerm
pmTerm = 
  do reserved "[π]"
     pm <- natw
     input <- cterm
     func <- cterm
     return $ Pm pm input func
     
paTerm ∷ Parser ITerm
paTerm = 
  do reserved "/\\"
     pa <- natw
     input <- cterm
     func <- cterm
     return $ Pa pa input func
     
npmTerm :: Parser ITerm
npmTerm =
  do reserved "\\/"
     input1 <- cterm
     input2 <- cterm
     return $ NPm input1 input2

boundTerm ∷ Parser ITerm
boundTerm =
     do reserved "Bound"
        index <- natural
        return $ Bound (fromInteger index)

--Parser for the global free variable name
gName ∷ Parser String
gName =  parens gName
     <|> identifier
--Parser for Name data type
name ∷ Parser Name
name = do reserved "Local"
          index <- natural
          return $ Local (fromInteger index)
     <|> do reserved "Quote"
            index <- natural
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
        pi <- natw
        iterm <- term
        cTerm <- cterm
        eof
        return $ App pi iterm cTerm

parseWhole ∷ Parser a -> Parser a
parseWhole p =
     do whiteSpace
        t <- p
        whiteSpace
        eof
        return t

term ∷ Parser ITerm
term = parens term
     <|> sortTerm
     <|> appTerm
     <|> piTerm
     <|> pmTerm
     <|> paTerm
     <|> npmTerm
     <|> boundTerm --Bound var
     <|> freeTerm --Free var

cterm ∷ Parser CTerm
cterm =  do reservedOp "Inf"
            iterm <- term
            return $ Inf iterm
     <|> do reservedOp "Lam"
            pi <- natw
            cTerm <- cterm
            return $ Lam pi cTerm

parseString ∷ Parser a → String → a
parseString p str =
     case parse p "" str of
     Left e -> error $ show e
     Right r -> r