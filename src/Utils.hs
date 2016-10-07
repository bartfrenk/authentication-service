module Utils where

import Control.Monad.Except (MonadError, throwError)

failWithC
  :: MonadError e m
  => e -> m (Maybe a) -> m a
failWithC = undefined

withExceptC
  :: MonadError e m
  => (e' -> e) -> m (Either e' a) -> m a
withExceptC f act = do
  eith <- act
  case eith of
    Left err -> throwError (f err)
    Right val -> return val
