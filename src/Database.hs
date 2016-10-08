module Database where

import Data.Text

import Control.Monad.Trans (MonadIO)
import Control.Monad (void)
import Entities
import Database.Persist.Sql

type UserName = Text

getUserByName
  :: MonadIO m
  => Text -> SqlPersistT m (Maybe User)
getUserByName nm = do
  user <- getBy $ UniqueName nm
  return $ entityVal `fmap` user

storeUser :: MonadIO m => User -> SqlPersistT m ()
storeUser user = void (insert user)
