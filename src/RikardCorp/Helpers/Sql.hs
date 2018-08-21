{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module RikardCorp.Helpers.Sql where

import           Database.Persist.MySQL
import           Control.Monad.IO.Class

type SqlM a = forall m. MonadIO m => SqlPersistT m a

sqlKeyInt :: ToBackendKey SqlBackend record => Key record -> Int
sqlKeyInt = fromIntegral . fromSqlKey
