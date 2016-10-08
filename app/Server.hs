{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Wai.Handler.Warp (run)

import Database.Persist.Postgresql
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Servant

import API
import Handlers
import Orphans ()
import Entities

main :: IO ()
main = do
  migratePostgreSql
  putStrLn "Authentication server"
  run tcpPort $ serve api (server postgres)

postgres :: ByteString
postgres =
  pack
    "host=localhost \
                \port=5432 \
                \user=auth \
                \password=auth \
                \dbname=auth"

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

-- TODO implement
toServantErr :: AuthErr -> ServantErr
toServantErr err = err500 { errBody = L.pack (show err) }

migratePostgreSql :: IO ()
migratePostgreSql =
  runStderrLoggingT $
  withPostgresqlPool postgres 10 $
  \pool -> liftIO $ runSqlPersistMPool (runMigration migrateAll) pool
