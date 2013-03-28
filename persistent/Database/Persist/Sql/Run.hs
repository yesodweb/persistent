module Database.Persist.Sql.Run where

import Database.Persist

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
runSqlPool :: MonadBaseControl IO m => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withResource pconn $ runSqlConn r

runSqlConn :: MonadBaseControl IO m => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = R.getStmt' conn
    liftBase $ begin conn getter
    x <- onException
            (runReaderT r conn)
            (liftBase $ rollbackC conn getter)
    liftBase $ commitC conn getter
    return x

runSqlPersistM :: SqlPersistM a -> Connection -> IO a
runSqlPersistM x conn = runResourceT $ runNoLoggingT $ runReaderT (unSqlPersistT x) conn
