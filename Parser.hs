module Parser where 

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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



arithmOperators = [ [Prefix (reservedOp "-"  >> return (Negate  ))          ]
                  , [Infix  (reservedOp "*"  >> return (Multiply)) AssocLeft]
                  , [Infix  (reservedOp "/"  >> return (Divide  )) AssocLeft]
                  , [Infix  (reservedOp "+"  >> return (Add     )) AssocLeft]
                  , [Infix  (reservedOp "-"  >> return (Subtract)) AssocLeft]
                  ]
 
boolOperators = [ [Prefix (reservedOp "!"   >> return (Not))          ]
                , [Infix  (reservedOp "&&"  >> return (And)) AssocLeft]
                , [Infix  (reservedOp "||"  >> return (Or )) AssocLeft]
                ]


arithmExpr :: Parser ArithmExpr
arithmExpr = buildExpressionParser arithmOperators arithmTerm

boolExpr :: Parser BoolExpr
boolExpr = buildExpressionParser boolOperators boolTerm

arithmTerm = parens arithmExpr 
         <|> liftM IntConst integer
         <|> liftM Var identifier
         <?> "simple expression"


boolTerm = parens boolExpr 
       <|> (string "false" >> return BoolFalse)
       <|> (string "true"  >> return BoolTrue)
       <?> "boolean expression"

