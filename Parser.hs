module Parser where 

import Text.ParserCombinators.Parsec hiding (token, tokens)
--import qualified Text.ParserCombinators

{-
expr    = buildExpressionParser table term
        <?> "expression"

term    =  parens expr 
        <|> natural
        <?> "simple expression"

table :: OperatorTable Char () Integer
table   = [ [prefix "-" negate, prefix "+" id ]
          , [postfix "++" (+1)]
          , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
          , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
          ]
        where
          binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
          prefix  name fun       = Prefix (do{ reservedOp name; return fun })
          postfix name fun       = Postfix (do{ reservedOp name; return fun })

-}
