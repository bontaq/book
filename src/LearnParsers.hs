module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser b
one' = one >> stop

one'' :: Parser ()
one'' = oneTwo >> eof

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse' :: Show a => Parser a -> IO ()
testParse' p =
  print $ parseString p mempty "123"

strParse :: Parser String
strParse =


pNL :: [Char] -> IO ()
pNL s  =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse' one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

  pNL "string:"
  testParse' strParse
