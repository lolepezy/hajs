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


dotExpr :: Expr -> Parser Expr
dotExpr e = (reservedOp "." >> (liftM (DotRef e) identifier)) <?> "object.property"

propRefExpr :: Expr -> Parser Expr
propRefExpr e = brackets (liftM (PropRef e) expr) <?> "object[property]"




