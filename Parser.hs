module Parser where 

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Applicative ((<*), (*>), (<$>), (<*>))

import Ast

languageDef = 
  emptyDef { Token.commentStart = "/*"
                , Token.commentEnd      = "*/"
                , Token.commentLine     = "//"
                , Token.identStart      = letter
                , Token.identLetter     = alphaNum
                , Token.reservedNames   = [ "if"
                                          , "else"
                                          , "while"
                                          , "do"
                                          , "continue"
                                          , "true"
                                          , "false"
                                          , "function"
                                          , "var"
                                          , "null"
                                          , "this"
                                          , "false"
                                                                  , "true"
                                                                  , "new"
                                                                  , "this"
                                                                  ]
                , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                                                  , "<", ">", "==", "!="
                                  , "&&", "||", "!", "."
                                                                  ]
                }

lexer = Token.makeTokenParser languageDef


identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol     lexer
brackets   = Token.brackets   lexer
braces     = Token.braces     lexer
comma      = Token.comma      lexer
colon      = Token.colon      lexer


allOperators = [ [Prefix (reservedOp "-"   >> return (UnaryOp Negate) )          ]
               , [Prefix (reservedOp "!"   >> return (UnaryOp Not ))          ]
               , [Infix  (reservedOp "*"   >> return (BinaryOp Multiply)) AssocLeft]
               , [Infix  (reservedOp "/"   >> return (BinaryOp Divide  )) AssocLeft]
               , [Infix  (reservedOp "+"   >> return (BinaryOp Add     )) AssocLeft]
               , [Infix  (reservedOp "-"   >> return (BinaryOp Subtract)) AssocLeft]
               , [Infix  (reservedOp "&&"  >> return (BinaryOp And)) AssocLeft]
               , [Infix  (reservedOp "||"  >> return (BinaryOp Or )) AssocLeft]
               , [Infix  (reservedOp "<"   >> return (RelOp Less))      AssocLeft]
               , [Infix  (reservedOp ">"   >> return (RelOp Greater))   AssocLeft]
               , [Infix  (reservedOp "=="  >> return (RelOp Equals))    AssocLeft]
               , [Infix  (reservedOp "===" >> return (RelOp EEquals))   AssocLeft]
               , [Infix  (reservedOp "!="  >> return (RelOp NotEquals)) AssocLeft]
               ]
 

expr :: Parser Expr
expr = buildExpressionParser allOperators exprTerm

exprTerm :: Parser Expr
exprTerm = parens expr 
         <|> (try (reserved "false") >> return BoolFalse)
         <|> (try (reserved "true" ) >> return BoolTrue)
         <|> liftM IntConst integer
         <|> liftM Var identifier
         <?> "simple expression"

primExpr :: Parser Expr
primExpr = simpleExpr <|> funcExpr <|> objLiteral

simpleExpression = (reserved "this" >> return ThisRef)
               <|> (reserved "null" >> return Null)  
               <|> (try (reserved "false") >> return BoolFalse)
               <|> (try (reserved "true" ) >> return BoolTrue)
               <|> liftM IntConst integer
               <|> liftM Var identifier
               <|> arrayLiteral


dotExpr :: Expr -> Parser Expr
dotExpr e = (reservedOp "." >> liftM (DotRef e) identifier) <?> "object.property"

propRefExpr :: Expr -> Parser Expr
propRefExpr e = brackets (liftM (PropRef e) expr) <?> "object[property]"

funcAppExpr :: Expr -> Parser Expr
funcAppExpr e = parens (liftM (FuncApp e) (sepBy expr comma)) <?> "function application"

objLiteral :: Parser Expr
objLiteral = braces (liftM Object (sepEndBy (do 
      prop <- stringLiteral <|> liftM Var identifier 
      colon 
      val <- expr
      return (prop, val)) comma)) <?> "object literal"

stringLiteral :: Parser Expr
stringLiteral = do
    q <- oneOf "'\""
    s <- many $ chars q
    char q <?> "closing quote"
    return $ StringConst s
  where
    chars q = escaped q <|> noneOf [q]
    escaped q = char '\\' >> choice (zipWith escapedChar (codes q) (replacements q))
    escapedChar code replacement = char code >> return replacement
    codes q        = ['b',  'n',  'f',  'r',  't',  '\\', q]
    replacements q = ['\b', '\n', '\f', '\r', '\t', '\\', q]

arrayLiteral :: Parser Expr
arrayLiteral = liftM Var identifier

simpleExpr :: Parser Expr
simpleExpr = objLiteral

funcExpr :: Parser Expr
funcExpr = objLiteral



