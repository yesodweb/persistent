{-# LANGUAGE ScopedTypeVariables #-}
module Database.Persist.Sql.Run where

import Control.Exception (bracket, mask, onException)
import Control.Monad (liftM)
import Control.Monad.IO.Unlift
import qualified UnliftIO.Exception as UE
import Control.Monad.Logger.CallStack
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as MonadReader
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.Resource
import Data.Acquire (Acquire, ReleaseType(..), mkAcquireType, with)
import Data.IORef (readIORef)
import Data.Pool (Pool, LocalPool)
import Data.Pool as P
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Timeout (timeout)

import Database.Persist.Class.PersistStore
import Database.Persist.Sql.Types
import Database.Persist.Sql.Types.Internal (IsolationLevel)
import Database.Persist.Sql.Raw

-- | The returned 'Acquire' gets a connection from the pool, but does __NOT__
-- start a new transaction. Used to implement 'acquireSqlConnFromPool' and
-- 'acquireSqlConnFromPoolWithIsolation', this is useful for performing actions
-- on a connection that cannot be done within a transaction, such as VACUUM in
-- Sqlite.
--
-- @since 2.10.5
unsafeAcquireSqlConnFromPool
    :: forall backend m
     . (MonadReader (Pool backend) m, BackendCompatible SqlBackend backend)
    => m (Acquire backend)
unsafeAcquireSqlConnFromPool = do
    pool <- MonadReader.ask

    let freeConn :: (backend, LocalPool backend) -> ReleaseType -> IO ()
        freeConn (res, localPool) relType = case relType of
            ReleaseException -> P.destroyResource pool localPool res
            _ -> P.putResource localPool res

    return $ fst <$> mkAcquireType (P.takeResource pool) freeConn

{-# DEPRECATED unsafeAcquireSqlConnFromPool "The Pool ~> Acquire functions are unpredictable and may result in resource leaks with asynchronous exceptions. They will be removed in 2.12. If you need them, please file an issue and we'll try to help get you sorted. See issue #1199 on GitHub for the debugging log." #-}


-- | The returned 'Acquire' gets a connection from the pool, starts a new
-- transaction and gives access to the prepared connection.
--
-- When the acquired connection is released the transaction is committed and
-- the connection returned to the pool.
--
-- Upon an exception the transaction is rolled back and the connection
-- destroyed.
--
-- This is equivalent to 'runSqlPool' but does not incur the 'MonadUnliftIO'
-- constraint, meaning it can be used within, for example, a 'Conduit'
-- pipeline.
--
-- @since 2.10.5
acquireSqlConnFromPool
    :: (MonadReader (Pool backend) m, BackendCompatible SqlBackend backend)
    => m (Acquire backend)
acquireSqlConnFromPool = do
    connFromPool <- unsafeAcquireSqlConnFromPool
    return $ connFromPool >>= acquireSqlConn

{-# DEPRECATED acquireSqlConnFromPool "The Pool ~> Acquire functions are unpredictable and may result in resource leaks with asynchronous exceptions. They will be removed in 2.12. If you need them, please file an issue and we'll try to help get you sorted. See issue #1199 on GitHub for the debugging log." #-}
--
-- | Like 'acquireSqlConnFromPool', but lets you specify an explicit isolation
-- level.
--
-- @since 2.10.5
acquireSqlConnFromPoolWithIsolation
    :: (MonadReader (Pool backend) m, BackendCompatible SqlBackend backend)
    => IsolationLevel -> m (Acquire backend)
acquireSqlConnFromPoolWithIsolation isolation = do
    connFromPool <- unsafeAcquireSqlConnFromPool
    return $ connFromPool >>= acquireSqlConnWithIsolation isolation

{-# DEPRECATED acquireSqlConnFromPoolWithIsolation "The Pool ~> Acquire functions are unpredictable and may result in resource leaks with asynchronous exceptions. They will be removed in 2.12. If you need them, please file an issue and we'll try to help get you sorted. See issue #1199 on GitHub for the debugging log." #-}

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
--
-- Note: This function previously timed out after 2 seconds, but this behavior
-- was buggy and caused more problems than it solved. Since version 2.1.2, it
-- performs no timeout checks.
runSqlPool
    :: (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> m a
runSqlPool r pconn =
    withRunInIO $ \runInIO ->
        withResource pconn $ \conn -> do
            let sqlBackend = projectBackend conn
            let getter = getStmtConn sqlBackend
            connBegin sqlBackend getter Nothing
            a <- runInIO (runReaderT r conn)
                `UE.catchAny` \e -> do
                    connRollback sqlBackend getter
                    UE.throwIO e
            connCommit sqlBackend getter
            pure a

-- | Like 'runSqlPool', but supports specifying an isolation level.
--
-- @since 2.9.0
runSqlPoolWithIsolation
    :: (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> IsolationLevel -> m a
runSqlPoolWithIsolation r pconn i =
    withRunInIO $ \runInIO ->
        withResource pconn $ \conn -> do
            let sqlBackend = projectBackend conn
            let getter = getStmtConn sqlBackend
            connBegin sqlBackend getter (Just i)
            a <- runInIO (runReaderT r conn)
                `UE.catchAny` \e -> do
                    connRollback sqlBackend getter
                    UE.throwIO e
            connCommit sqlBackend getter
            pure a

rawAcquireSqlConn
    :: forall backend m
     . (MonadReader backend m, BackendCompatible SqlBackend backend)
    => Maybe IsolationLevel -> m (Acquire backend)
rawAcquireSqlConn isolation = do
    conn <- MonadReader.ask
    let rawConn :: SqlBackend
        rawConn = projectBackend conn

        getter :: T.Text -> IO Statement
        getter = getStmtConn rawConn

        beginTransaction :: IO backend
        beginTransaction = conn <$ connBegin rawConn getter isolation

        finishTransaction :: backend -> ReleaseType -> IO ()
        finishTransaction _ relType = case relType of
            ReleaseException -> connRollback rawConn getter
            _ -> connCommit rawConn getter

    return $ mkAcquireType beginTransaction finishTransaction

-- | Starts a new transaction on the connection. When the acquired connection
-- is released the transaction is committed and the connection returned to the
-- pool.
--
-- Upon an exception the transaction is rolled back and the connection
-- destroyed.
--
-- This is equivalent to 'runSqlConn but does not incur the 'MonadUnliftIO'
-- constraint, meaning it can be used within, for example, a 'Conduit'
-- pipeline.
--
-- @since 2.10.5
acquireSqlConn
    :: (MonadReader backend m, BackendCompatible SqlBackend backend)
    => m (Acquire backend)
acquireSqlConn = rawAcquireSqlConn Nothing

-- | Like 'acquireSqlConn', but lets you specify an explicit isolation level.
--
-- @since 2.10.5
acquireSqlConnWithIsolation
    :: (MonadReader backend m, BackendCompatible SqlBackend backend)
    => IsolationLevel -> m (Acquire backend)
acquireSqlConnWithIsolation = rawAcquireSqlConn . Just

runSqlConn :: (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> backend -> m a
runSqlConn r conn = with (acquireSqlConn conn) $ runReaderT r

-- | Like 'runSqlConn', but supports specifying an isolation level.
--
-- @since 2.9.0
runSqlConnWithIsolation :: (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> backend -> IsolationLevel -> m a
runSqlConnWithIsolation r conn isolation =
  with (acquireSqlConnWithIsolation isolation conn) $ runReaderT r

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
