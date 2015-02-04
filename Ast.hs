{-# LANGUAGE GADTs #-}
module Ast where 

import Text.ParserCombinators.Parsec

data ExprPos = ExprPos Expr SourcePos 
  deriving (Show, Eq)

getExpr :: ExprPos -> Expr
getExpr (ExprPos e _) = e

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


