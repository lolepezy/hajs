module Executor where

data Program

data Value = SVal String | IVal Int

data Var = String Value

data Context = Context {
  vars :: [Var]
}

execute :: Program -> Context -> Context
execute p c = c

