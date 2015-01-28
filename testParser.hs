import Ast
import Parser

import Text.ParserCombinators.Parsec
import Test.HUnit

testParse parser = parse parser "parser"

literalTest = TestList [
                       ]

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

