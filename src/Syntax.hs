module Syntax where

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (State)

import Picalc

run :: String -> IO (Pi Value)
run s = eval (parseString pikaParser s)

parseString :: Parser a -> String -> a
parseString p str =
   case parse p "" str of
     Left e  -> error $ show e
     Right r -> r

reserved :: [String]
reserved = ["par","0","!"]

identifier = Token.identifier lexer -- parses an identifier
reservedToken = Token.reserved lexer -- parses a reserved name
whiteSpace = Token.whiteSpace lexer

languageDef = emptyDef { 
             Token.reservedNames   = reserved
            }
ints = Token.integer lexer
parens = Token.parens lexer
angles = Token.angles lexer
lexer = Token.makeTokenParser languageDef

pikaParser :: Parser (Term Value)
pikaParser = do 
    e <- whiteSpace >> parseExpr 
    return $ Term e []  

parseExpr :: Parser (Pi Value)
parseExpr =  parseZero
         <|> try parseNew 
         <|> try parseRecv 
         <|> try parseSend 
         <|> parseBang
         <|> parsePeek
         <|> parsePar
          

parseBang :: Parser (Pi Value)
parseBang = do 
    char '!'
    n <- many1 digit
    char '.'
    many space 
    p <- parseExpr
    return (Bang (read n) p)

parsePar :: Parser (Pi Value)
parsePar = do
    string "par"
    many space
    char '('
    many space
    p1 <- parseExpr
    many space
    char '|'
    many space
    p2 <- parseExpr
    many space
    char ')'
    return (Par p1 p2)  
      
parseNew :: Parser (Pi Value)
parseNew = do 
    nu <- parens (char 'v' >> many space >> many1 letter)
    char '.'
    many space 
    p <- parseExpr
    return $ New nu p

parseZero :: Parser (Pi Value) 
parseZero = do
    char '0'
    return Zero

parseMsg :: Parser (Msg Value)
parseMsg = parseVar <|> parseConst 

parseVar :: Parser (Msg Value)
parseVar = do
    v <- identifier
    return (Var v)

parsePeek :: Parser (Pi Value) 
parsePeek = do
    c <- parseMsg
    return (Peek c)
    
parseConst :: Parser (Msg Value)
parseConst = do 
    n <- ints
    return (Const (VInt n))
    
parseSend :: Parser (Pi Value)
parseSend = do
    m1 <- identifier
    m2 <- angles parseMsg
    k  <- optionMaybe (char '.' >> whiteSpace >> parseExpr)
    return $ case k of
        Nothing -> Send m1 m2 Zero
        Just p  -> Send m1 m2 p

parseRecv :: Parser (Pi Value) 
parseRecv = do 
    m1 <- identifier
    m2 <- parens (many1 letter) 
    char '.'
    many space
    p <- parseExpr
    return $ Recv m1 m2 p

