module Scanner where 

import Data

import Data.Char
import Text.ParserCombinators.Parsec hiding (token, tokens)
import Control.Applicative ((<*), (*>), (<$>), (<*>))


data Token = Identifier String 
  | SConstant String
  | IConstant Integer
  | Operator
  | Var
  | Function
  | If
  | Else
  | While
  | Do
  | Return
  | Comma
  | Colon
  | Plus
  | Minus
  | Star
  | Slash
  | Dot
  | LBracket | RBracket 
  | LPar | RPar
  | LCurly | RCurly
  deriving (Show, Eq)

type TokenPos = (Token, SourcePos)

parsePos :: Parser Token -> Parser TokenPos
parsePos p = (,) <$> p <*> getPosition 

ident :: Parser TokenPos 
ident = parsePos $ do
  c <- letter 
  rest <- many (alphaNum <|> char '_')
  return $ Identifier (c:rest)

 
stringConst :: Parser TokenPos
stringConst = parsePos $ do
    q <- oneOf "'\""
    s <- many $ chars q
    char q <?> "closing quote"
    return $ SConstant s
  where
    chars q = escaped q <|> noneOf [q]
    escaped q = char '\\' >> choice (zipWith escapedChar (codes q) (replacements q))
    escapedChar code replacement = char code >> return replacement
    codes q        = ['b',  'n',  'f',  'r',  't',  '\\', q]
    replacements q = ['\b', '\n', '\f', '\r', '\t', '\\', q]
 

intConst :: Parser TokenPos
intConst = parsePos (minInt <|> plusInt <|> justDigits <?> "integer value")
  where 
    minInt = (IConstant . negate . toInteger) <$> (char '-' *> many1 digit)
    plusInt = (IConstant . toInteger) <$> (char '+' *> many1 digit)
    justDigits = (IConstant . toInteger) <$> many1 digit
    toInteger s = fromIntegral $ foldl (\a i -> a * 10 + digitToInt i) 0 s
    
   
primitives :: Parser TokenPos
primitives = parsePos $ choice $ map (\(ch, tok) -> char ch >> return tok) [
    ( ';', Colon ), ( ',', Comma ), ( '.', Dot ),
    ( '+', Plus ), ( '-', Minus ), ( '*', Star ), ('/', Slash),
    ( '(', LPar ), ( ')', RPar ), 
    ( '[', LBracket ), ( ']', RBracket ), 
    ( '{', LCurly ), ( '}', RCurly )
  ]

reserved :: Parser TokenPos
reserved = parsePos $ choice $ map (\(s, tok) -> string s >> return tok) [
    ( "var", Var ), ( "function", Function ), ( "if", If ), ("else", Else),
    ( "while", While ), ( "do", Do ), ( "return", Return )
  ]


aToken :: Parser TokenPos
aToken = choice 
    [ primitives,
      reserved,
      intConst,
      stringConst,
      ident 
    ]

tokenStream :: Parser [TokenPos]
tokenStream = spaces *> many (aToken <* spaces) 

