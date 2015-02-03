import Ast
import Parser

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser = parse parser "scanner"

testParser parser string expression = TestCase $ assertBool 
  ("Should get " ++ show expression ++ " after parsing " ++ string ++ " but was \n" ++ show parseResult)
  (case parseResult of
     Right (tok, s) -> tok == expression
     Left err -> False
  )
  where parseResult = testParse parser string


literalTest = TestList [{-testParser expr "\"xy\"" (StringConst "xy"),
                        testParser expr ("\"x" ++ "\\\"" ++ "y\"") (StringConst "x\"y"), 
                        testParser expr "'xy'" (StringConst "xy"), 
                        testParser expr "'x\\'y'" (StringConst "x'y"),
                        testParser expr "1234" (IntConst 1234),
                        testParser expr "-91" (IntConst $ -91)-}
                       ]

main = runTestTT literalTest

{-
refTest :: Parser Expr
refTest = do 
  i <- expr
  dr <- dotExpr i <|> propRefExpr i <|> funcAppExpr i
  return dr

main = do
--  print $ testParse expr "1+ -2"                
--  print $ testParse expr "(1+2-z) / a" 
  print $ testParse expr "1+2" 

  print $ testParse refTest "(1+x).a" 
  print $ testParse refTest "(1+x)[a * 2]" 
  print $ testParse refTest "(1+x)(z, false)" 

  print $ testParse objLiteral "{k : (1+x), 'qq' : 15 }" 
-- runTestTT literalTest
-}
