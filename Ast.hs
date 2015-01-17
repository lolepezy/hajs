module Ast where 

data Expr = IntConst Integer
  | StringConst String
  | BoolFalse
  | BoolTrue
  | Var String
  | UnaryOp UOp Expr
  | BinaryOp BOp Expr Expr
  | RelOp ROp Expr Expr
  deriving (Show, Eq)

data UOp = Negate | Not 
  deriving (Show, Eq)

data BOp = And | Or | Add | Multiply | Subtract | Divide 
  deriving (Show, Eq)

data ROp = Less | Greater | Equals | NotEquals | EEquals 
  deriving (Show, Eq)


