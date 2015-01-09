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
  "Parsed the stuff wrong: " 
   token (case (testParse parser string) of 
       Right (t, s) -> t
    )

literalTest = TestList [testScanner stringConst "\"xy\"" (SConstant "xy"),
                        testScanner stringConst ("\"x" ++ ['\\', '"'] ++ "y\"") (SConstant "x\"y"), 
                        testScanner stringConst ['\'', 'x', 'y', '\''] (SConstant "xy"), 
                        testScanner stringConst ['\'', 'x', '\\', '\'', 'y', '\''] (SConstant "x'y"),
 
                        testScanner intConst "1234" (IConstant 1234),
                        testScanner intConst "-91" (IConstant $ -91), 
                        testScanner intConst "+9" (IConstant $ 9),

                        testScanner primitives "," Comma,
                        testScanner primitives ";" Colon,
                        testScanner primitives "." Dot,
                        testScanner reserved "return" Return
                       ]

main = runTestTT literalTest

