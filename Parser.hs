module Parser where 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef = 
  emptyDef { Token.commentStart    = "/*"
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
								  ]
		, Token.reservedOpNames = ["+", "-", "*", "/", "="
								  , "<", ">", "&&", "||", "!", "."
								  ]
		}

lexer = Token.makeTokenParser languageDef


identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace





