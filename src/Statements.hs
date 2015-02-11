module Statements where

import           Control.Applicative                    ((*>), (<$>), (<*),
                                                         (<*>))
import           Control.Monad
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

import           Ast
import           Expressions

singleStatement :: Parser (WithPos Statement)
singleStatement = do
    a <- varDeclaration <|> assignment
    optional eos
    return a
    
statement :: Parser (WithPos Statement)
statement = singleStatement <|> ifStatement <|> whileStatement <|> block

varDeclaration :: Parser (WithPos Statement)
varDeclaration = do
    pos <- getPosition    
    var <- reserved "var" *> identifier
    ex <- optionMaybe $ reservedOp "=" *> expr
    optional eos
    case ex of 
        Just e -> return $ WithPos (Assign (LVar var) (term e)) pos
        Nothing -> return $ WithPos (VarDecl var) pos

assignment :: Parser (WithPos Statement)
assignment = do
    pos <- getPosition
    var <- identifier <* reservedOp "=" <?> "an identifier or property reference"
    e <- expr
    return $ WithPos (Assign (LVar var) (term e)) pos

block :: Parser (WithPos Statement)
block = withPosL Block (braces (many singleStatement))

ifStatement :: Parser (WithPos Statement)
ifStatement = do
    pos <- getPosition
    condition <- reserved "if" *> parens expr <?> "condition expression in 'if' statement"
    thenBlock <- statement
    elseBlock <- optionMaybe $ reservedOp "else" *> statement
    return $ WithPos (If (term condition) (term thenBlock) (liftM term elseBlock)) pos

whileStatement :: Parser (WithPos Statement)
whileStatement = do
    pos <- getPosition
    condition <- reserved "while" *> parens expr <?> "condition expression in 'while' statement"
    b <- statement
    return $ WithPos (While (term condition) (term b)) pos

eos = withSpaces (string ";") <|> withSpaces (string "\n") <|> withSpaces (string "\n\r")
