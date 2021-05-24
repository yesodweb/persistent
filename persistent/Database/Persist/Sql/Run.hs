{-# LANGUAGE ScopedTypeVariables #-}
module Database.Persist.Sql.Run where

import Control.Monad.IO.Unlift
import qualified UnliftIO.Exception as UE
import Control.Monad.Logger.CallStack
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as MonadReader
import Control.Monad.Trans.Reader hiding (local)
import Control.Monad.Trans.Resource
import Data.Acquire (Acquire, ReleaseType(..), mkAcquireType, with)
import Data.IORef (readIORef)
import Data.Pool as P
import qualified Data.Map as Map
import qualified Data.Text as T

import Database.Persist.Class.PersistStore
import Database.Persist.Sql.Types
import Database.Persist.Sql.Types.Internal
import Database.Persist.Sql.Raw

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
--
-- This function performs the given action in a transaction. If an
-- exception occurs during the action, then the transaction is rolled back.
--
-- Note: This function previously timed out after 2 seconds, but this behavior
-- was buggy and caused more problems than it solved. Since version 2.1.2, it
-- performs no timeout checks.
runSqlPool
    :: forall backend m a. (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> m a
runSqlPool r pconn = do
    rawRunSqlPool r pconn Nothing

-- | Like 'runSqlPool', but supports specifying an isolation level.
--
-- @since 2.9.0
runSqlPoolWithIsolation
    :: forall backend m a. (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> IsolationLevel -> m a
runSqlPoolWithIsolation r pconn i =
    rawRunSqlPool r pconn (Just i)

-- | Like 'runSqlPool', but does not surround the action in a transaction.
-- This action might leave your database in a weird state.
--
-- @since 2.12.0.0
runSqlPoolNoTransaction
    :: forall backend m a. (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> Maybe IsolationLevel -> m a
runSqlPoolNoTransaction r pconn i =
    runSqlPoolWithHooks r pconn i (\_ -> pure ()) (\_ -> pure ()) (\_ _ -> pure ())

rawRunSqlPool
    :: forall backend m a. (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a -> Pool backend -> Maybe IsolationLevel -> m a
rawRunSqlPool r pconn mi =
    runSqlPoolWithHooks r pconn mi before after onException
  where
    before conn = do
        let sqlBackend = projectBackend conn
        let getter = getStmtConn sqlBackend
        liftIO $ connBegin sqlBackend getter mi
    after conn = do
        let sqlBackend = projectBackend conn
        let getter = getStmtConn sqlBackend
        liftIO $ connCommit sqlBackend getter
    onException conn _ = do
        let sqlBackend = projectBackend conn
        let getter = getStmtConn sqlBackend
        liftIO $ connRollback sqlBackend getter

-- | This function is how 'runSqlPool' and 'runSqlPoolNoTransaction' are
-- defined. In addition to the action to be performed and the 'Pool' of
-- conections to use, we give you the opportunity to provide three actions
-- - initialize, afterwards, and onException.
--
-- @since 2.12.0.0
runSqlPoolWithHooks
    :: forall backend m a before after onException. (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m a
    -> Pool backend
    -> Maybe IsolationLevel
    -> (backend -> m before)
    -- ^ Run this action immediately before the action is performed.
    -> (backend -> m after)
    -- ^ Run this action immediately after the action is completed.
    -> (backend -> UE.SomeException -> m onException)
    -- ^ This action is performed when an exception is received. The
    -- exception is provided as a convenience - it is rethrown once this
    -- cleanup function is complete.
    -> m a
runSqlPoolWithHooks r pconn i before after onException =
    withRunInIO $ \runInIO ->
    withResource pconn $ \conn ->
    UE.mask $ \restore -> do
        _ <- restore $ runInIO $ before conn
        a <- restore (runInIO (runReaderT r conn))
            `UE.catchAny` \e -> do
                _ <- restore $ runInIO $ onException conn e
                UE.throwIO e
        _ <- restore $ runInIO $ after conn
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
            ReleaseException -> do
                connRollback rawConn getter
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

runSqlConn :: forall backend m a. (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> backend -> m a
runSqlConn r conn = with (acquireSqlConn conn) $ runReaderT r

-- | Like 'runSqlConn', but supports specifying an isolation level.
--
-- @since 2.9.0
runSqlConnWithIsolation :: forall backend m a. (MonadUnliftIO m, BackendCompatible SqlBackend backend) => ReaderT backend m a -> backend -> IsolationLevel -> m a
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
    :: forall backend m a. (MonadIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend (NoLoggingT (ResourceT IO)) a -> Pool backend -> m a
liftSqlPersistMPool x pool = liftIO (runSqlPersistMPool x pool)

withSqlPool
    :: forall backend m a. (MonadLoggerIO m, MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend) -- ^ create a new connection
    -> Int -- ^ connection count
    -> (Pool backend -> m a)
    -> m a
withSqlPool mkConn connCount f = withSqlPoolWithConfig mkConn (defaultConnectionPoolConfig { connectionPoolConfigSize = connCount } ) f

-- | Creates a pool of connections to a SQL database which can be used by the @Pool backend -> m a@ function.
-- After the function completes, the connections are destroyed.
--
-- @since 2.11.0.0
withSqlPoolWithConfig
    :: forall backend m a. (MonadLoggerIO m, MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend) -- ^ Function to create a new connection
    -> ConnectionPoolConfig
    -> (Pool backend -> m a)
    -> m a
withSqlPoolWithConfig mkConn poolConfig f = withUnliftIO $ \u -> UE.bracket
    (unliftIO u $ createSqlPoolWithConfig mkConn poolConfig)
    destroyAllResources
    (unliftIO u . f)

createSqlPool
    :: forall backend m. (MonadLoggerIO m, MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend)
    -> Int
    -> m (Pool backend)
createSqlPool mkConn size = createSqlPoolWithConfig mkConn (defaultConnectionPoolConfig { connectionPoolConfigSize = size } )

-- | Creates a pool of connections to a SQL database.
--
-- @since 2.11.0.0
createSqlPoolWithConfig
    :: forall m backend. (MonadLoggerIO m, MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend) -- ^ Function to create a new connection
    -> ConnectionPoolConfig
    -> m (Pool backend)
createSqlPoolWithConfig mkConn config = do
    logFunc <- askLoggerIO
    -- Resource pool will swallow any exceptions from close. We want to log
    -- them instead.
    let loggedClose :: backend -> IO ()
        loggedClose backend = close' backend `UE.catchAny` \e -> do
            runLoggingT
              (logError $ T.pack $ "Error closing database connection in pool: " ++ show e)
              logFunc
            UE.throwIO e
    liftIO $ createPool
        (mkConn logFunc)
        loggedClose
        (connectionPoolConfigStripes config)
        (connectionPoolConfigIdleTimeout config)
        (connectionPoolConfigSize config)

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
    :: forall backend m a. (MonadUnliftIO m, MonadLoggerIO m, BackendCompatible SqlBackend backend)
    => (LogFunc -> IO backend) -> (backend -> m a) -> m a
withSqlConn open f = do
    logFunc <- askLoggerIO
    withRunInIO $ \run -> UE.bracket
      (open logFunc)
      close'
      (run . f)

close' :: (BackendCompatible SqlBackend backend) => backend -> IO ()
close' conn = do
    readIORef (connStmtMap $ projectBackend conn) >>= mapM_ stmtFinalize . Map.elems
    connClose $ projectBackend conn
