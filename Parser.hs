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
               where prefOp op f = Prefix ( do { pos <- getPosition; reservedOp op; return (\e -> MkExprPos (f (getExpr e)) pos) } <?> "prefix operator")
                     infOp op f accoc = Infix ( do
                                                   pos <- getPosition 
                                                   reservedOp op 
                                                   return (\e1 e2 -> MkExprPos (f (getExpr e1) (getExpr e2)) pos) <?> "infix operator") accoc


-- some utilities
withPos constructor parser = do { pos <- getPosition; e <- parser; return $ MkExprPos (constructor e) pos }
withPosP constructor parser = do { pos <- getPosition; e <- parser; return $ MkExprPos (constructor (getExpr e)) pos }
withPosL constructor parser = do { pos <- getPosition; es <- parser; return $ MkExprPos (constructor (map getExpr es)) pos }
withPos0 constructor parser = do { pos <- getPosition; parser; return $ MkExprPos constructor pos }


expr :: Parser ExprPos
expr = buildExpressionParser allOperators exprTerm

exprTerm :: Parser ExprPos
exprTerm = parens expr
         <|> withPos0 BoolFalse (try(reserved "false"))
         <|> withPos0 BoolTrue (try(reserved "true"))
         <|> withPos IntConst integer
         <|> withPos Var identifier
         <?> "simple expression"


primExpr :: Parser ExprPos
primExpr = simpleExpr <|> funcExpr <|> objLiteral

simpleExpression = withPos0 ThisRef (reserved "this")
               <|> withPos0 Null (reserved "null")
               <|> withPos0 BoolFalse (try(reserved "false"))
               <|> withPos0 BoolTrue (try (reserved "true" ))
               <|> withPos IntConst integer
               <|> withPos Var identifier
               <|> arrayLiteral


dotExpr :: ExprPos -> Parser ExprPos
dotExpr e = withPos (DotRef (getExpr e)) (reservedOp "." >> identifier) <?> "object.property"

propRefExpr :: ExprPos -> Parser ExprPos
propRefExpr e = withPosP (PropRef (getExpr e)) (brackets expr) <?> "object[property]"

funcAppExpr :: ExprPos -> Parser ExprPos
funcAppExpr e = withPosL (FuncApp (getExpr e)) (parens (sepBy expr comma)) <?> "function application"

objLiteral :: Parser ExprPos
objLiteral = withPos Object (braces (sepEndBy (do
      prop <- stringLiteral <|> withPos Var identifier
      colon
      val <- expr
      return (getExpr prop, getExpr val)) comma)) <?> "object literal"

stringLiteral :: Parser ExprPos
stringLiteral = do
    pos <- getPosition
    q <- oneOf "'\""
    s <- many $ chars q
    char q <?> "closing quote"
    return $ MkExprPos (StringConst s) pos
  where
    chars q = escaped q <|> noneOf [q]
    escaped q = char '\\' >> choice (zipWith escapedChar (codes q) (replacements q))
    escapedChar code replacement = char code >> return replacement
    codes q        = ['b',  'n',  'f',  'r',  't',  '\\', q]
    replacements q = ['\b', '\n', '\f', '\r', '\t', '\\', q]

arrayLiteral :: Parser ExprPos
arrayLiteral = withPos Var identifier

simpleExpr :: Parser ExprPos
simpleExpr = objLiteral

funcExpr :: Parser ExprPos
funcExpr = objLiteral


