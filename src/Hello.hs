module Hello where

twoo :: IO Bool
twoo = do
  c  <- getChar
  c' <- getChar
  return (c == c')

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hi " ++ name ++ "!")
