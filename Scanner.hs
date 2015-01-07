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
    q <- oneOf "'\""
    s <- many $ chars q
    char q
    return $ (SConstant s, pos)
  where
    chars q = (escaped q) <|> noneOf [q]
    escaped q = char '\\' >> choice (zipWith escapedChar (codes q) (replacements q))
    escapedChar code replacement = char code >> return replacement
    codes q        = ['b',  'n',  'f',  'r',  't',  '\\', q]
    replacements q = ['\b', '\n', '\f', '\r', '\t', '\\', q]
 


