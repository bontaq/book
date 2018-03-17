{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Network.Wai
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
    , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let oldVal = fromMaybe 0 (M.lookup k m)
      newVal = oldVal + 1
  in (M.insert k newVal m, newVal)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key' = mappend (prefix config) unprefixed
        m' = (counts config)
    unpacked <- liftIO $ readIORef m'
    let (newMap, value) = bumpBoomp key' unpacked
    _ <- liftIO $ (writeIORef m' newMap)
    html $
      mconcat [ "<h1>success. count was: "
              , TL.pack $ show value
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let
    config = Config counter (TL.pack prefixArg)
    runR r = runReaderT r config
  scottyT 3000 runR app
