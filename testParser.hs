import Parser

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser = parse parser "parser"

literalTest = TestList [
                       ]

main = do
  print $ testParse arithmExpr "1+2" 
  print $ testParse arithmExpr "(1+2) / a" 
-- runTestTT literalTest

