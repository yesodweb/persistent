{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sql.Run where

import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Data.Conduit.Pool
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.Base
import Control.Exception.Lifted (onException)
import Control.Monad.IO.Class
import Control.Exception.Lifted (bracket)
import Data.IORef (readIORef)
import qualified Data.Map as Map
import Control.Exception.Lifted (throwIO)

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
runSqlPool :: MonadBaseControl IO m => SqlPersistT m a -> Pool Connection -> m a
runSqlPool r pconn = do
    mres <- withResourceTimeout 2000000 pconn $ runSqlConn r
    maybe (throwIO Couldn'tGetSQLConnection) return mres

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

withSqlPool :: MonadIO m
            => IO Connection -- ^ create a new connection
            -> Int -- ^ connection count
            -> (Pool Connection -> m a)
            -> m a
withSqlPool mkConn connCount f = do
    pool <- createSqlPool mkConn connCount
    f pool

createSqlPool :: MonadIO m
              => IO Connection
              -> Int
              -> m (Pool Connection)
createSqlPool mkConn = liftIO . createPool mkConn close' 1 20

withSqlConn :: (MonadIO m, MonadBaseControl IO m)
            => IO Connection -> (Connection -> m a) -> m a
withSqlConn open = bracket (liftIO open) (liftIO . close')

close' :: Connection -> IO ()
close' conn = do
    readIORef (connStmtMap conn) >>= mapM_ stmtFinalize . Map.elems
    connClose conn
