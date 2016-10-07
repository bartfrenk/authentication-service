{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import Control.Monad.Except
import Control.Monad.Logger
import Crypto.Random (MonadRandom(..))

instance MonadRandom m =>
         MonadRandom (ExceptT e m) where
  getRandomBytes n = lift (getRandomBytes n)

instance MonadRandom m =>
         MonadRandom (LoggingT m) where
  getRandomBytes n = lift (getRandomBytes n)
