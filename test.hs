import Scanner

import Text.ParserCombinators.Parsec

testParse parser string = parse parser "scanner" string

main = do
--  print $ testParse ident "a"
--  print $ testParse ident "1"
--  print $ testParse ident "a1"
--  print $ testParse ident "a a"
  print $ testParse stringConst "'aa'"
  print $ testParse stringConst "'a'a'"

