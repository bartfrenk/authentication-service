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

import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger, logInfo)
import Control.Monad.Trans (MonadIO)
import Crypto.Random (MonadRandom)
import Data.Text (append)

import Authentication
import Entities (userToken)
import Utils (failWithC)
import Store (MonadAuthStore(..))

handleAuthenticate
  :: (MonadAuthStore m
     ,MonadError AuthErr m
     ,MonadIO m
     ,MonadLogger m
     ,MonadRandom m)
  => Credentials -> m AuthToken
handleAuthenticate creds@(Credentials nm _) = do
  $(logInfo) $ append "Attempting to authenticate user " nm
  user <- failWithC (UnknownUserName nm) (getUserByName nm)
  authenticate user creds

handleRegister
  :: (MonadAuthStore m
     ,MonadError AuthErr m
     ,MonadIO m
     ,MonadLogger m
     ,MonadRandom m)
  => Credentials -> m UserToken
handleRegister creds@(Credentials nm _) = do
  $(logInfo) $ append "attempting to register user " nm
  user <- register creds
  storeUser user
  return (UserToken $ userToken user)
