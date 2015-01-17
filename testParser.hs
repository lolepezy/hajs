import Parser

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser = parse parser "parser"

literalTest = TestList [
                       ]

main = do
  print $ testParse expr "1+ -2" 
  print $ testParse expr "(1+2-z) / a" 
  print $ testParse expr "true1" 
-- runTestTT literalTest

