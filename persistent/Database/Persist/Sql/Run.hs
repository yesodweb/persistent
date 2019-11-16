{-# LANGUAGE ScopedTypeVariables #-}
module Database.Persist.Sql.Run where

import Control.Exception (bracket, mask, onException)
import Control.Monad (liftM)
import Control.Monad.IO.Unlift
import qualified UnliftIO.Exception as UE
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.Resource
import Data.IORef (readIORef)
import Data.Pool as P
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Timeout (timeout)

import Database.Persist.Class.PersistStore
import Database.Persist.Sql.Types
import Database.Persist.Sql.Types.Internal (IsolationLevel)
import Database.Persist.Sql.Raw

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
--
-- Note: This function previously timed out after 2 seconds, but this behavior
-- was buggy and caused more problems than it solved. Since version 2.1.2, it
-- performs no timeout checks.
runSqlPool
    :: (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> m a
runSqlPool r pconn = withRunInIO $ \run -> withResource pconn $ run . runSqlConn r

-- | Like 'runSqlPool', but supports specifying an isolation level.
--
-- @since 2.9.0
runSqlPoolWithIsolation
    :: (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> IsolationLevel -> m a
runSqlPoolWithIsolation r pconn i = withRunInIO $ \run -> withResource pconn $ run . (\conn -> runSqlConnWithIsolation r conn i)

-- | Like 'withResource', but times out the operation if resource
-- allocation does not complete within the given timeout period.
--
-- @since 2.0.0
withResourceTimeout
  :: forall a m b.  (MonadUnliftIO m)
  => Int -- ^ Timeout period in microseconds
  -> Pool a
  -> (a -> m b)
  -> m (Maybe b)
{-# SPECIALIZE withResourceTimeout :: Int -> Pool a -> (a -> IO b) -> IO (Maybe b) #-}
withResourceTimeout ms pool act = withRunInIO $ \runInIO -> mask $ \restore -> do
    mres <- timeout ms $ takeResource pool
    case mres of
        Nothing -> runInIO $ return (Nothing :: Maybe b)
        Just (resource, local) -> do
            ret <- restore (runInIO (liftM Just $ act resource)) `onException`
                    destroyResource pool local resource
            putResource local resource
            return ret
{-# INLINABLE withResourceTimeout #-}

runSqlConn :: (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> backend -> m a
runSqlConn r conn = withRunInIO $ \runInIO -> mask $ \restore -> do
    let conn' = projectBackend conn
        getter = getStmtConn conn'
    restore $ connBegin conn' getter Nothing
    x <- onException
            (restore $ runInIO $ runReaderT r conn)
            (restore $ connRollback conn' getter)
    restore $ connCommit conn' getter
    return x

-- | Like 'runSqlConn', but supports specifying an isolation level.
--
-- @since 2.9.0
runSqlConnWithIsolation :: (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> backend -> IsolationLevel -> m a
runSqlConnWithIsolation r conn isolation = withRunInIO $ \runInIO -> mask $ \restore -> do
    let conn' = projectBackend conn
        getter = getStmtConn conn'
    restore $ connBegin conn' getter $ Just isolation
    x <- onException
            (restore $ runInIO $ runReaderT r conn)
            (restore $ connRollback conn' getter)
    restore $ connCommit conn' getter
    return x

runSqlPersistM
    :: (BackendCompatible SqlBackend backend)
    => ReaderT backend (NoLoggingT (ResourceT IO)) a -> backend -> IO a
runSqlPersistM x conn = runResourceT $ runNoLoggingT $ runSqlConn x conn

runSqlPersistMPool
    :: (BackendCompatible SqlBackend backend)
    => ReaderT backend (NoLoggingT (ResourceT IO)) a -> Pool backend -> IO a
runSqlPersistMPool x pool = runResourceT $ runNoLoggingT $ runSqlPool x pool

liftSqlPersistMPool
    :: (MonadIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend (NoLoggingT (ResourceT IO)) a -> Pool backend -> m a
liftSqlPersistMPool x pool = liftIO (runSqlPersistMPool x pool)

withSqlPool
    :: (MonadLogger m, MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend) -- ^ create a new connection
    -> Int -- ^ connection count
    -> (Pool backend -> m a)
    -> m a
withSqlPool mkConn connCount f = withUnliftIO $ \u -> bracket
    (unliftIO u $ createSqlPool mkConn connCount)
    destroyAllResources
    (unliftIO u . f)

createSqlPool
    :: forall m backend. (MonadLogger m, MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend)
    -> Int
    -> m (Pool backend)
createSqlPool mkConn size = do
    logFunc <- askLogFunc
    -- Resource pool will swallow any exceptions from close. We want to log
    -- them instead.
    let loggedClose :: backend -> IO ()
        loggedClose backend = close' backend `UE.catchAny` \e -> runLoggingT
          (logError $ T.pack $ "Error closing database connection in pool: " ++ show e)
          logFunc
    liftIO $ createPool (mkConn logFunc) loggedClose 1 20 size

-- NOTE: This function is a terrible, ugly hack. It would be much better to
-- just clean up monad-logger.
--
-- FIXME: in a future release, switch over to the new askLoggerIO function
-- added in monad-logger 0.3.10. That function was not available at the time
-- this code was written.
askLogFunc :: forall m. (MonadUnliftIO m, MonadLogger m) => m LogFunc
askLogFunc = withRunInIO $ \run ->
    return $ \a b c d -> run (monadLoggerLog a b c d)

-- | Create a connection and run sql queries within it. This function
-- automatically closes the connection on it's completion.
--
-- === __Example usage__
--
-- > {-# LANGUAGE GADTs #-}
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TypeFamilies#-}
-- > {-# LANGUAGE TemplateHaskell#-}
-- > {-# LANGUAGE QuasiQuotes#-}
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- >
-- > import Control.Monad.IO.Class  (liftIO)
-- > import Control.Monad.Logger
-- > import Conduit
-- > import Database.Persist
-- > import Database.Sqlite
-- > import Database.Persist.Sqlite
-- > import Database.Persist.TH
-- >
-- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- > Person
-- >   name String
-- >   age Int Maybe
-- >   deriving Show
-- > |]
-- >
-- > openConnection :: LogFunc -> IO SqlBackend
-- > openConnection logfn = do
-- >  conn <- open "/home/sibi/test.db"
-- >  wrapConnection conn logfn
-- >
-- > main :: IO ()
-- > main = do
-- >   runNoLoggingT $ runResourceT $ withSqlConn openConnection (\backend ->
-- >                                       flip runSqlConn backend $ do
-- >                                         runMigration migrateAll
-- >                                         insert_ $ Person "John doe" $ Just 35
-- >                                         insert_ $ Person "Divya" $ Just 36
-- >                                         (pers :: [Entity Person]) <- selectList [] []
-- >                                         liftIO $ print pers
-- >                                         return ()
-- >                                      )
--
-- On executing it, you get this output:
--
-- > Migrating: CREATE TABLE "person"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"age" INTEGER NULL)
-- > [Entity {entityKey = PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = Person {personName = "John doe", personAge = Just 35}},Entity {entityKey = PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 2}}, entityVal = Person {personName = "Hema", personAge = Just 36}}]
--

withSqlConn
    :: (MonadUnliftIO m, MonadLogger m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend) -> (backend -> m a) -> m a
withSqlConn open f = do
    logFunc <- askLogFunc
    withRunInIO $ \run -> bracket
      (open logFunc)
      close'
      (run . f)

close' :: (BackendCompatible SqlBackend backend) => backend -> IO ()
close' conn = do
    readIORef (connStmtMap $ projectBackend conn) >>= mapM_ stmtFinalize . Map.elems
    connClose $ projectBackend conn
