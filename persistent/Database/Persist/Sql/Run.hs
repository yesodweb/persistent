{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sql.Run where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Data.Conduit.Pool
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.Base
import Control.Exception.Lifted (onException)

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
runSqlPool :: MonadBaseControl IO m => SqlPersistT m a -> Pool Connection -> m a
runSqlPool r pconn = withResource pconn $ runSqlConn r

runSqlConn :: MonadBaseControl IO m => SqlPersistT m a -> Connection -> m a
runSqlConn (SqlPersistT r) conn = do
    let getter = getStmtConn conn
    liftBase $ connBegin conn getter
    x <- onException
            (runReaderT r conn)
            (liftBase $ connRollback conn getter)
    liftBase $ connCommit conn getter
    return x

runSqlPersistM :: SqlPersistM a -> Connection -> IO a
runSqlPersistM x conn = runResourceT $ runNoLoggingT $ runReaderT (unSqlPersistT x) conn
