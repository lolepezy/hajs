module Expressions where

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
                                          , "var"                                          
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
               where prefOp op f = Prefix ( do { pos <- getPosition; reservedOp op; return (\e -> WithPos (f (term e)) pos) } <?> "prefix operator")
                     infOp op f  = Infix ( do 
                         pos <- getPosition
                         reservedOp op
                         return (\e1 e2 -> WithPos (f (term e1) (term e2)) pos) <?> "infix operator")


expr :: Parser (WithPos Expr)
expr = buildExpressionParser allOperators exprTerm

exprTerm :: Parser (WithPos Expr)
exprTerm = parens expr
         <|> withPos0 BoolFalse (try(reserved "false"))
         <|> withPos0 BoolTrue (try(reserved "true"))
         <|> withPos IntConst integer
         <|> withPos Var identifier
         <|> withPos StringConst stringLiteral
         <|> objLiteral
         <|> arrayLiteral
         <?> "simple expression"

simpleExpression = withPos0 ThisRef (reserved "this")
               <|> withPos0 Null (reserved "null")
               <|> withPos0 BoolFalse (try(reserved "false"))
               <|> withPos0 BoolTrue (try (reserved "true" ))
               <|> withPos IntConst integer
               <|> withPos Var identifier
               <|> arrayLiteral


dotExpr :: (WithPos Expr) -> Parser (WithPos Expr)
dotExpr e = withPos (DotRef (term e)) (reservedOp "." >> identifier) <?> "object.property"

propRefExpr :: (WithPos Expr) -> Parser (WithPos Expr)
propRefExpr e = withPosP (PropRef (term e)) (brackets expr) <?> "object[property]"

funcAppExpr :: (WithPos Expr) -> Parser (WithPos Expr)
funcAppExpr e = withPosL (FuncApp (term e)) (parens (sepBy expr comma)) <?> "function application"

objLiteral :: Parser (WithPos Expr)
objLiteral = let 
    obj = braces (sepEndBy (do
      prop <- stringLiteral <|> identifier
      colon
      val <- expr
      return (prop, term val)) comma)
    in withPos Object obj <?> "object literal"

arrayLiteral :: Parser (WithPos Expr)
arrayLiteral = withPosL Array (brackets (expr `sepBy` comma)) <?> "array literal"

stringLiteral :: Parser String
stringLiteral = do
    q <- oneOf "'\""
    s <- many $ chars q
    char q <?> "closing quote"
    return s
  where
    chars q = escaped q <|> noneOf [q]
    escaped q = char '\\' >> choice (zipWith escapedChar (codes q) (replacements q))
    escapedChar code replacement = char code >> return replacement
    codes q        = ['b',  'n',  'f',  'r',  't',  '\\', q]
    replacements q = ['\b', '\n', '\f', '\r', '\t', '\\', q]

simpleExpr :: Parser (WithPos Expr)
simpleExpr = objLiteral

withSpaces :: Parser e -> Parser e
withSpaces parser = whitespaces *> parser <* whitespaces 
  where whitespaces = many $ oneOf " \t"

