{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)

import Database.Persist.Postgresql
import Data.ByteString (ByteString)
import Servant

import API
import Handlers
import Orphans ()

main :: IO ()
main = putStrLn "Authentication server"

server :: ByteString -> Server AuthenticationAPI
server connStr = enter (Nat run) (handleAuthenticate :<|> handleRegister)
  where
    run = withExceptT toServantErr . runStdoutLoggingT . runPostgreSql connStr 10

runPostgreSql
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
  => ByteString -> Int -> SqlPersistT m a -> m a
runPostgreSql connStr pools = withPostgresqlPool connStr pools . runSqlPool

toServantErr :: AuthErr -> ServantErr
toServantErr = undefined
