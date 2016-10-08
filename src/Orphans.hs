{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where


import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Crypto.Random (MonadRandom(..))

instance MonadRandom m =>
         MonadRandom (ExceptT e m) where
  getRandomBytes n = lift (getRandomBytes n)

instance MonadRandom m =>
         MonadRandom (LoggingT m) where
  getRandomBytes n = lift (getRandomBytes n)

instance MonadRandom m =>
         MonadRandom (ReaderT e m) where
  getRandomBytes n = lift (getRandomBytes n)
