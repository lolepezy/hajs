import Ast
import Expressions
import Statements

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

arrayLitTest = TestList [ testParser expr "[1, 'a', qq*3]" $ Array [IntConst 1, StringConst "a", (BinaryOp Multiply (Var "qq") (IntConst 3))]
                        ]

statementTest = TestList [ testParser statement "{}" $ Block [],
                           testParser statement "x = 1" $ Assign (LVar "x") (IntConst 1),
                           testParser statement "x = 1;" $ Assign (LVar "x") (IntConst 1),
                           testParser statement "var x = 10;" $ Assign (LVar "x") (IntConst 10),
                           testParser statement "variablex = 10;" $ Assign (LVar "variablex") (IntConst 10),
                           testParser statement "{ x = 1; }" $ Block [Assign (LVar "x") (IntConst 1)],
                           testParser statement "{ x = 1; zz = 2 }" $ Block [Assign (LVar "x") (IntConst 1), Assign (LVar "zz") (IntConst 2)],
                           testParser block "{ x = 1 \n zz = 2 }" $ Block [Assign (LVar "x") (IntConst 1), Assign (LVar "zz") (IntConst 2)]                           
                         ]

ifTest = TestList [ let condition = RelOp Equals (Var "a") (IntConst 1)
                        ifBlock = Block [Assign (LVar "x") (IntConst 2)]
                        elseBlock = Block [Assign (LVar "z") (IntConst 15)]
                        in testParser statement "if (a == 1) { x = 2 } else { z = 15 }" $ If condition ifBlock (Just elseBlock),
                    let condition = RelOp Equals (Var "a") (IntConst 1)
                        ifBlock = Assign (LVar "x") (IntConst 2)
                        elseBlock = Assign (LVar "z") (IntConst 15)
                        in testParser statement "if (a == 1) x = 2; else z = 15;" $ If condition ifBlock (Just elseBlock) 
                    ]
                        
whileTest = TestList [ let condition = RelOp Less (Var "a") (Var "x") 
                           block = Block [Assign (LVar "a") (BinaryOp Multiply (IntConst 2) (Var "a"))]
                        in testParser statement "while (a < x) { a = 2*a; } " $ While condition block
                        ]

main = runTestTT $ TestList [
    literalsTest, arithmTest, objLitTest, arrayLitTest,
    statementTest, ifTest, whileTest
    ]

