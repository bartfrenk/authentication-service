{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Wai.Handler.Warp (run)

import Database.Persist.Postgresql
import Data.ByteString (ByteString)
import Servant

import API
import Handlers
import Orphans ()

main :: IO ()
main = do
  putStrLn "Authentication server"
  run tcpPort $ serve api (server "asdsa")

server :: ByteString -> Server AuthenticationAPI
server connStr = enter (Nat f) (handleAuthenticate :<|> handleRegister)
  where
    f = withExceptT toServantErr . runStdoutLoggingT . runPostgreSql connStr 10

tcpPort :: Int
tcpPort = 8000

runPostgreSql
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => ByteString -> Int -> SqlPersistT m a -> m a
runPostgreSql connStr pools = withPostgresqlPool connStr pools . runSqlPool

toServantErr :: AuthErr -> ServantErr
toServantErr = undefined
