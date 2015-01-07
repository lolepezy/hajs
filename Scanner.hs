module Scanner where 

import Data

import Text.ParserCombinators.Parsec hiding (token, tokens)

data Token = Identifier String 
  | SConstant String
  | IConstant Integer
  | Operator
  | Reserved String
  | LCurly | RCurly
  | Function
  | If
  | While
  | Comma
  | Var
  | Dot
  | LBraket | RBraket
  deriving (Show, Eq)

type TokenPos = (Token, SourcePos)

ident :: Parser TokenPos 
ident = do
  pos <- getPosition 
  c <- letter 
  rest <- many (alphaNum <|> char '_')
  return $ (Identifier (c:rest), pos)  

 
stringConst :: Parser TokenPos
stringConst = do 
  pos <- getPosition 
  string "'"
  s <- manyTill anyChar (try (string "'"))
  return $ (SConstant s, pos)




