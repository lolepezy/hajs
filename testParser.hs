import Ast
import Expressions

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser = parse parser "parser"

testParser parser string expression = TestCase $ assertBool 
  ("Should get " ++ show expression ++ " after parsing " ++ string ++ " but was \n" ++ show parseResult)
  (case parseResult of
     Right (WithPos tok pos) -> tok == expression
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

arithmTest = TestList [testParser expr "x+1" (BinaryOp Add (Var "x") (IntConst 1) ),
                       testParser expr "x*y+2-q" (BinaryOp Subtract (BinaryOp Add (BinaryOp Multiply (Var "x") (Var "y")) (IntConst 2)) (Var "q")),
                       testParser expr "x&&y || !z" (BinaryOp Or (BinaryOp And (Var "x") (Var "y")) (UnaryOp Not (Var "z")))
                       ]

objLitTest = TestList [ testParser expr "{a:x}" $ Object [("a", Var "x")],
                        let objB = Object [("c", (BinaryOp Add (Var "d") (IntConst 2) ))]
                            in testParser expr "{a : x, b : {c : d + 2}}" (Object [("a", Var "x"), ("b", objB )])
                      ]

arrayLitTest = TestList [ testParser expr "[1, a, qq*3]" $ Array [IntConst 1, Var "a", (BinaryOp Multiply (Var "qq") (IntConst 3))]
                        ]


main = runTestTT $ TestList [literalsTest, arithmTest, objLitTest, arrayLitTest]

