module Parser where

import           Control.Applicative                    ((*>), (<$>), (<*),
                                                         (<*>))
import           Control.Monad
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

import           Ast

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

allOperators = [ [prefOp "-"   (UnaryOp Negate)            ]
               , [prefOp "!"   (UnaryOp Not)               ]
               , [infOp  "*"   (BinaryOp Multiply) AssocLeft, infOp  "/" (BinaryOp Divide) AssocLeft]
               , [infOp  "+"   (BinaryOp Add) AssocLeft, infOp  "-" (BinaryOp Subtract) AssocLeft]
               , [infOp  "&&"  (BinaryOp And) AssocLeft]
               , [infOp  "||"  (BinaryOp Or)  AssocLeft]
               , [infOp  "<"   (RelOp Less)  AssocLeft]
               , [infOp  ">"   (RelOp Greater)  AssocLeft]
               , [infOp  "=="  (RelOp Equals)  AssocLeft]
               , [infOp  "===" (RelOp EEquals)  AssocLeft]
               , [infOp  "!="  (RelOp NotEquals)  AssocLeft]
               ]
               where prefOp op f = Prefix ( do { pos <- getPosition; reservedOp op; return (\e -> f e pos) } <?> "prefix operator")
                     infOp op f accoc = Infix ( do { pos <- getPosition; reservedOp op; return (\e1 e2 -> f e1 e2 pos) } <?> "infix operator") accoc


-- some utilities
withPos constructor parser = do { pos <- getPosition; e <- parser; return $ constructor e pos }
withPos0 constructor parser = do { pos <- getPosition; parser; return $ constructor pos }


expr :: Parser Expr
expr = buildExpressionParser allOperators exprTerm

exprTerm :: Parser Expr
exprTerm = parens expr
         <|> withPos0 BoolFalse (try(reserved "false"))
         <|> withPos0 BoolTrue (try(reserved "true"))
         <|> withPos IntConst integer
         <|> withPos Var identifier
         <?> "simple expression"


primExpr :: Parser Expr
primExpr = simpleExpr <|> funcExpr <|> objLiteral

simpleExpression = withPos0 ThisRef (reserved "this")
               <|> withPos0 Null (reserved "null")
               <|> withPos0 BoolFalse (try(reserved "false"))
               <|> withPos0 BoolTrue (try (reserved "true" ))
               <|> withPos IntConst integer
               <|> withPos Var identifier
               <|> arrayLiteral


dotExpr :: Expr -> Parser Expr
dotExpr e = withPos (DotRef e) (reservedOp "." >> identifier) <?> "object.property"

propRefExpr :: Expr -> Parser Expr
propRefExpr e = withPos (PropRef e) (brackets expr) <?> "object[property]"

funcAppExpr :: Expr -> Parser Expr
funcAppExpr e = withPos (FuncApp e) (parens (sepBy expr comma)) <?> "function application"

objLiteral :: Parser Expr
objLiteral = withPos Object (braces (sepEndBy (do
      prop <- stringLiteral <|> withPos Var identifier
      colon
      val <- expr
      return (prop, val)) comma)) <?> "object literal"

stringLiteral :: Parser Expr
stringLiteral = do
    pos <- getPosition
    q <- oneOf "'\""
    s <- many $ chars q
    char q <?> "closing quote"
    return $ StringConst s pos
  where
    chars q = escaped q <|> noneOf [q]
    escaped q = char '\\' >> choice (zipWith escapedChar (codes q) (replacements q))
    escapedChar code replacement = char code >> return replacement
    codes q        = ['b',  'n',  'f',  'r',  't',  '\\', q]
    replacements q = ['\b', '\n', '\f', '\r', '\t', '\\', q]

arrayLiteral :: Parser Expr
arrayLiteral = withPos Var identifier

simpleExpr :: Parser Expr
simpleExpr = objLiteral

funcExpr :: Parser Expr
funcExpr = objLiteral


