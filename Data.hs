module Data where 

data BoolExpr = BoolConst Bool
  | Not BoolExpr
  | BoolBinaryOp BoolOp BoolExpr BoolExpr
  deriving (Show)

data BoolOp = And | Or deriving (Show)

data GenExpr = Var String 
  | FunCall [GenExpr]
  | Const ConstValue
  | GenBinary GenExpr GenExpr
  | Neg GenExpr
  deriving (Show)

data ConstValue = Integer | String deriving (Show)

data Statement = Seq [Statement]
  | Assign String GenExpr
  deriving (Show)



