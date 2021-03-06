import Scanner
import Expressions

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser = parse parser "scanner"

testScanner parser string token = TestCase $ assertBool 
  ("Should get " ++ show token ++ " after parsing " ++ string ++ " but was \n" ++ show parseResult)
  (case parseResult of
     Right (tok, s) -> tok == token
     Left err -> False
  )
  where parseResult = testParse parser string


testScannerList parser string tokens = TestCase $ assertBool 
  ("Should get " ++ show tokens ++ " after parsing " ++ string ++ " but was \n" ++ show parseResult)
  (case parseResult of 
     Right ts@(_:_) -> map fst ts == tokens
     Left err -> False
  )
  where parseResult = testParse parser string




literalTest = TestList [testScanner stringConst "\"xy\"" (SConstant "xy"),
                        testScanner stringConst ("\"x" ++ "\\\"" ++ "y\"") (SConstant "x\"y"), 
                        testScanner stringConst "'xy'" (SConstant "xy"), 
                        testScanner stringConst "'x\\'y'" (SConstant "x'y"),
 
                        testScanner intConst "1234" (IConstant 1234),
                        testScanner intConst "-91" (IConstant $ -91), 

                        testScanner primitives "," Comma,
                        testScanner primitives ";" Colon,
                        testScanner primitives "." Dot,
                        testScanner reserved "return" Return,

                        testScannerList tokenStream "\"1234\".length+44-'zz'" 
                          [SConstant "1234", Dot, Identifier "length", Plus, 
                           IConstant 44, Minus, SConstant "zz" ]
                       ]

main = runTestTT literalTest

