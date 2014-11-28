{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sql.Run where

import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Control.Monad.Trans.Control
import Data.Pool as P
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.Base
import Control.Exception.Lifted (onException)
import Control.Monad.IO.Class
import Control.Exception.Lifted (bracket)
import Control.Exception (mask)
import System.Timeout (timeout)
import Control.Monad.Trans.Control (control)
import Data.IORef (readIORef)
import qualified Data.Map as Map
import Control.Exception.Lifted (throwIO)
import Control.Exception (mask)
import Control.Monad (liftM)
import System.Timeout (timeout)

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
runSqlPool :: MonadBaseControl IO m => SqlPersistT m a -> Pool SqlBackend -> m a
runSqlPool r pconn = do
    mres <- withResourceTimeout 2000000 pconn $ runSqlConn r
    maybe (throwIO Couldn'tGetSQLConnection) return mres

-- | Like 'withResource', but times out the operation if resource
-- allocation does not complete within the given timeout period.
--
-- Since 2.0.0
withResourceTimeout ::
    (MonadBaseControl IO m)
  => Int -- ^ Timeout period in microseconds
  -> Pool a
  -> (a -> m b)
  -> m (Maybe b)
{-# SPECIALIZE withResourceTimeout :: Int -> Pool a -> (a -> IO b) -> IO (Maybe b) #-}
withResourceTimeout ms pool act = control $ \runInIO -> mask $ \restore -> do
    mres <- timeout ms $ takeResource pool
    case mres of
        Nothing -> runInIO $ return Nothing
        Just (resource, local) -> do
            ret <- restore (runInIO (liftM Just $ act resource)) `onException`
                    destroyResource pool local resource
            putResource local resource
            return ret
{-# INLINABLE withResourceTimeout #-}

runSqlConn :: MonadBaseControl IO m => SqlPersistT m a -> SqlBackend -> m a
runSqlConn r conn = do
    let getter = getStmtConn conn
    liftBase $ connBegin conn getter
    x <- onException
            (runReaderT r conn)
            (liftBase $ connRollback conn getter)
    liftBase $ connCommit conn getter
    return x

runSqlPersistM :: SqlPersistM a -> SqlBackend -> IO a
runSqlPersistM x conn = runResourceT $ runNoLoggingT $ runSqlConn x conn

runSqlPersistMPool :: SqlPersistM a -> Pool SqlBackend -> IO a
runSqlPersistMPool x pool = runResourceT $ runNoLoggingT $ runSqlPool x pool

withSqlPool :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
            => (LogFunc -> IO SqlBackend) -- ^ create a new connection
            -> Int -- ^ connection count
            -> (Pool SqlBackend -> m a)
            -> m a
withSqlPool mkConn connCount f = do
    pool <- createSqlPool mkConn connCount
    f pool

createSqlPool :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
              => (LogFunc -> IO SqlBackend)
              -> Int
              -> m (Pool SqlBackend)
createSqlPool mkConn size = do
    logFunc <- askLogFunc
    liftIO $ createPool (mkConn logFunc) close' 1 20 size

-- NOTE: This function is a terrible, ugly hack. It would be much better to
-- just clean up monad-logger.
--
-- FIXME: in a future release, switch over to the new askLoggerIO function
-- added in monad-logger 0.3.10. That function was not available at the time
-- this code was written.
askLogFunc :: (MonadBaseControl IO m, MonadLogger m) => m LogFunc
askLogFunc = do
    runInBase <- control $ \run -> run $ return run
    return $ \a b c d -> do
        _ <- runInBase (monadLoggerLog a b c d)
        return ()

withSqlConn :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
            => (LogFunc -> IO SqlBackend) -> (SqlBackend -> m a) -> m a
withSqlConn open f = do
    logFunc <- askLogFunc
    bracket (liftIO $ open logFunc) (liftIO . close') f

close' :: SqlBackend -> IO ()
close' conn = do
    readIORef (connStmtMap conn) >>= mapM_ stmtFinalize . Map.elems
    connClose conn
