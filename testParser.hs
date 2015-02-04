import Ast
import Parser

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser = parse parser "parser"

testParser parser string expression = TestCase $ assertBool 
  ("Should get " ++ show expression ++ " after parsing " ++ string ++ " but was \n" ++ show parseResult)
  (case parseResult of
     Right (ExprPos tok pos) -> tok == expression
     Left err -> False
  )
  where parseResult = testParse parser string


literalsTest = TestList [testParser expr "\"xy\"" (StringConst "xy"),
                        testParser expr ("\"x" ++ "\\\"" ++ "y\"") (StringConst "x\"y"), 
                        testParser expr "'xy'" (StringConst "xy"), 
                        testParser expr "'x\\'y'" (StringConst "x'y"),
                        testParser expr "1234" (IntConst 1234),
                        testParser expr "-91" (UnaryOp Negate (IntConst 91))
                       ]

main = runTestTT literalsTest

