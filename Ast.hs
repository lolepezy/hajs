{-# LANGUAGE GADTs #-}
module Ast where 

import Text.ParserCombinators.Parsec

data ExprPos e where 
    Ast :: e -> SourcePos -> ExprPos e
    deriving (Show, Eq)

data Expr = IntConst Integer SourcePos
  | StringConst String SourcePos
  | BoolFalse SourcePos
  | BoolTrue SourcePos
  | Null SourcePos
  | ThisRef SourcePos
  | Var String SourcePos
  | UnaryOp UOp Expr SourcePos
  | BinaryOp BOp Expr Expr SourcePos
  | RelOp ROp Expr Expr SourcePos
  | DotRef Expr String SourcePos
  | PropRef Expr Expr SourcePos
  | FuncApp Expr [Expr] SourcePos
  | NewOp Expr [Expr] SourcePos
  | Object [(Expr, Expr)] SourcePos
  deriving (Show, Eq)

data UOp = Negate | Not 
  deriving (Show, Eq)

data BOp = And | Or | Add | Multiply | Subtract | Divide 
  deriving (Show, Eq)

data ROp = Less | Greater | Equals | NotEquals | EEquals 
  deriving (Show, Eq)


