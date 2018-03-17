{-# LANGUAGE OverloadedStrings #-}

module HttpStuff where

import qualified Data.ByteString.Lazy as BS
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: IO [Response BS.ByteString]
mappingGet = traverse get urls

main :: IO ()
main = do
  xs <- mappingGet
  let t = fmap (\x -> show x) xs
--   ys <- fmap (fmap . Data.ByteString.Lazy.putStrLn) xs
  return ()
