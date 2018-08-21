{-# LANGUAGE FlexibleContexts #-}

module RikardCorp.Helpers.Exceptions where

  import qualified Control.Exception as E
  import qualified Control.Exception.Lifted as EL
  import Control.Monad.Trans.Control

  catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
  catchAny = E.catch

  liftedCatchAny :: (MonadBaseControl IO m) => m a -> (E.SomeException -> m a) -> m a
  liftedCatchAny = EL.catch
