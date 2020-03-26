module Syntax where

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (State)

import Picalc

run :: Read a => String -> IO (Pi a)
run s = eval (parseString pikaParser s)

parseString :: Parser a -> String -> a
parseString p str =
   case parse p "" str of
     Left e  -> error $ show e
     Right r -> r

reservados :: [String]
reservados = ["par","0","!"]

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
whiteSpace = Token.whiteSpace lexer

languageDef = emptyDef { 
             Token.reservedNames   = reservados
            }
ints = Token.integer lexer
parens = Token.parens lexer
angles = Token.angles lexer
lexer = Token.makeTokenParser languageDef

pikaParser :: Read a => Parser (Term a)
pikaParser = do 
    e <- whiteSpace >> parseExpr 
    return $ Term e []  

parseExpr :: Read a => Parser (Pi a)
parseExpr =  parseZero
         <|> try parseNew 
         <|> try parseRecv 
         <|> try parseSend 
         <|> parseBang
         <|> parsePeek
         <|> parsePar
          

parseBang :: Read a => Parser (Pi a)
parseBang = do 
    char '!'
    n <- many1 digit
    char '.'
    many space 
    p <- parseExpr
    return (Bang (read n) p)

parsePar :: Read a => Parser (Pi a)
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
      
parseNew :: Read a => Parser (Pi a)
parseNew = do 
    nu <- parens (char 'v' >> many space >> many1 letter)
    char '.'
    many space 
    p <- parseExpr
    return $ New nu p

parseZero :: Read a => Parser (Pi a) 
parseZero = do
    char '0'
    return Zero

parseMsg :: Read a => Parser (Msg a)
parseMsg = parseVar <|> parseConst 

parseVar :: Parser (Msg a)
parseVar = do
    v <- identifier
    return (Var v)

parsePeek :: Read a => Parser (Pi a) 
parsePeek = do
    c <- parseMsg
    return (Peek c)
    
parseConst :: Read a => Parser (Msg a)
parseConst = do 
    c <- many1 digit
    return (Const $ read c)
    
parseSend :: Read a => Parser (Pi a) 
parseSend = do 
    m1 <- identifier
    m2 <- angles parseMsg 
    char '.'
    many space
    p <- parseExpr
    return $ Send m1 m2 p

parseRecv :: Read a => Parser (Pi a) 
parseRecv = do 
    m1 <- identifier
    m2 <- parens (many1 letter) 
    char '.'
    many space
    p <- parseExpr
    return $ Recv m1 m2 p

