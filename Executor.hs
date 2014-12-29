module Executor where

data Program = ()

data Value = String

data Var = (String, Value)

data Context = {
  vars :: [Var]
}

execute :: Program -> Context -> Context
execute p c = c

