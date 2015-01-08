import Scanner

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser string = parse parser "scanner" string

--main = do
--  print $ testParse ident "a"
--  print $ testParse ident "1"
--  print $ testParse ident "a1"
--  print $ testParse ident "a a"


testScanner parser string token = TestCase $ assertEqual 
  ("Should get " ++ (show token) ++ " after parsing " ++ string) 
    token (case (testParse parser string) of 
       Right (t, s) -> t
    )

literalTest = TestList [testScanner stringConst "\"xy\"" (SConstant "xy"),
                        testScanner stringConst ("\"x" ++ ['\\', '"'] ++ "y\"") (SConstant "x\"y"), 
                        testScanner stringConst ['\'', 'x', 'y', '\''] (SConstant "xy"), 
                        testScanner stringConst ['\'', 'x', '\\', '\'', 'y', '\''] (SConstant "x'y") 
                       ]

main = runTestTT literalTest

