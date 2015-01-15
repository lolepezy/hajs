module Ast where 

data BoolExpr = BoolTrue 
  | BoolFalse
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or  BoolExpr BoolExpr
  deriving (Show, Eq)

data ArithmExpr = IntConst Integer
  | StringConst String
  | Var String
  | Negate ArithmExpr
  | Multiply ArithmExpr ArithmExpr
  | Add      ArithmExpr ArithmExpr
  | Divide   ArithmExpr ArithmExpr
  | Subtract ArithmExpr ArithmExpr
  deriving (Show, Eq)



