{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Store where

import Data.Text

import Control.Monad.Trans (MonadIO)
import Control.Monad (void)
import Entities
import Database.Persist.Sql

class Monad m =>
      MonadAuthStore m  where
  storeUser :: User -> m ()
  getUserByName :: Text -> m (Maybe User)


instance MonadIO m =>
         MonadAuthStore (SqlPersistT m) where
  getUserByName nm = do
    user <- getBy $ UniqueName nm
    return $ entityVal `fmap` user
  storeUser user = void (insert user)
