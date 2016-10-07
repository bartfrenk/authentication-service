{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers
  ( AuthErr
  , handleAuthenticate
  , handleRegister
  ) where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans (MonadIO)
import Crypto.Random (MonadRandom)
import Data.ByteString (ByteString)
import Data.Text
import Database.Persist.Sql

import Authentication
import Database
import Entities (userToken)
import Utils (failWithC)

handleAuthenticate
  :: (MonadError AuthErr m, MonadIO m, MonadLogger m, MonadRandom m)
  => Credentials -> SqlPersistT m AuthToken
handleAuthenticate creds@(Credentials nm _) = do
  $(logInfo) $ append "Attempting to authenticate user " nm
  user <- failWithC (UnknownUserName nm) (getUser nm)
  lift (authenticate user creds)

handleRegister
  :: (MonadError AuthErr m, MonadIO m, MonadLogger m, MonadRandom m)
  => Credentials -> SqlPersistT m ByteString
handleRegister creds@(Credentials nm _) = do
  $(logInfo) $ append "attempting to register user " nm
  user <- lift (register creds)
  storeUser user
  return (userToken user)
