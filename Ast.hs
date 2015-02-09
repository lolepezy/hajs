{-# LANGUAGE GADTs #-}
module Ast where 

import Text.ParserCombinators.Parsec

data Expr = IntConst Integer
  | StringConst String
  | BoolFalse
  | BoolTrue
  | Null
  | ThisRef
  | Var String
  | UnaryOp UOp Expr
  | BinaryOp BOp Expr Expr
  | RelOp ROp Expr Expr
  | DotRef Expr String
  | PropRef Expr Expr
  | FuncApp Expr [Expr]
  | NewOp Expr [Expr]
  | Object [(Expr, Expr)]
  deriving (Show, Eq)

data UOp = Negate | Not 
  deriving (Show, Eq)

data BOp = And | Or | Add | Multiply | Subtract | Divide 
  deriving (Show, Eq)

data ROp = Less | Greater | Equals | NotEquals | EEquals 
  deriving (Show, Eq)


{- Statements -}
data LExpr = LVar String
  | LPropRef Expr Expr
  | LDotRef Expr String

data Statement = Block [Statement]
  | Assign LExpr
  | If Expr Statement (Maybe Statement)
  | While Expr Statement 


data WithPos e = WithPos e SourcePos 
  deriving (Show, Eq)

term :: WithPos e -> e
term (WithPos e _) = e

withPos :: (a -> b) -> Parser a -> Parser (WithPos b)
withPos constructor parser = do 
    pos <- getPosition 
    ex <- parser 
    return $ WithPos (constructor ex) pos

withPosP constructor = withPos (constructor . term)
withPosL constructor = withPos (constructor . (map term))
withPos0 constructor = withPos (\e -> constructor)
