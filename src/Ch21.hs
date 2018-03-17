module Ch21 where

-- filterF :: (Applicative f, Foldable t, Mondoid (f a))
--         => (a -> Bool) -> t a -> f a

-- stringToMorse :: String -> Maybe [Morse]
-- stringToMorse s =
--   sequence $ fmap charToMorse s

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn
  :: Query
  -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a
