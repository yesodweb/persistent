{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Database.Persist.Sql.Types.Internal
    ( HasPersistBackend (..)
    , IsPersistBackend (..)
    , SqlReadBackend (..)
    , SqlWriteBackend (..)
    , readToUnknown
    , readToWrite
    , writeToUnknown
    , LogFunc
    , InsertSqlResult (..)
    , Statement (..)
    , IsolationLevel (..)
    , makeIsolationLevelStatement
    , SqlBackend (..)
    , SqlBackendCanRead
    , SqlBackendCanWrite
    , SqlReadT
    , SqlWriteT
    , IsSqlBackend
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LogSource, LogLevel, Loc)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Acquire (Acquire)
import Data.Conduit (ConduitM)
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import System.Log.FastLogger (LogStr)

import Database.Persist.Class
  ( HasPersistBackend (..)
  , PersistQueryRead, PersistQueryWrite
  , PersistStoreRead, PersistStoreWrite
  , PersistUniqueRead, PersistUniqueWrite
  , BackendCompatible(..)
  )
import Database.Persist.Class.PersistStore (IsPersistBackend (..))
import Database.Persist.Types

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data InsertSqlResult = ISRSingle Text
                     | ISRInsertGet Text Text
                     | ISRManyKeys Text [PersistValue]

data Statement = Statement
    { stmtFinalize :: IO ()
    , stmtReset :: IO ()
    , stmtExecute :: [PersistValue] -> IO Int64
    , stmtQuery :: forall m. MonadIO m
                => [PersistValue]
                -> Acquire (ConduitM () [PersistValue] m ())
    }

-- | Please refer to the documentation for the database in question for a full
-- overview of the semantics of the varying isloation levels
data IsolationLevel = ReadUncommitted
                    | ReadCommitted
                    | RepeatableRead
                    | Serializable
                    deriving (Show, Eq, Enum, Ord, Bounded)

makeIsolationLevelStatement :: (Monoid s, IsString s) => IsolationLevel -> s
makeIsolationLevelStatement l = "SET TRANSACTION ISOLATION LEVEL " <> case l of
    ReadUncommitted -> "READ UNCOMMITTED"
    ReadCommitted -> "READ COMMITTED"
    RepeatableRead -> "REPEATABLE READ"
    Serializable -> "SERIALIZABLE"

-- | A 'SqlBackend' represents a handle or connection to a database. It
-- contains functions and values that allow databases to have more
-- optimized implementations, as well as references that benefit
-- performance and sharing.
--
-- A 'SqlBackend' is *not* thread-safe. You should not assume that
-- a 'SqlBackend' can be shared among threads and run concurrent queries.
-- This *will* result in problems. Instead, you should create a @'Pool'
-- 'SqlBackend'@, known as a 'ConnectionPool', and pass that around in
-- multi-threaded applications.
--
-- To run actions in the @persistent@ library, you should use the
-- 'runSqlConn' function. If you're using a multithreaded application, use
-- the 'runSqlPool' function.
data SqlBackend = SqlBackend
    { connPrepare :: Text -> IO Statement
    -- ^ This function should prepare a 'Statement' in the target database,
    -- which should allow for efficient query reuse.
    , connInsertSql :: EntityDef -> [PersistValue] -> InsertSqlResult
    -- ^ This function generates the SQL and values necessary for
    -- performing an insert against the database.
    , connInsertManySql :: Maybe (EntityDef -> [[PersistValue]] -> InsertSqlResult)
    -- ^ SQL for inserting many rows and returning their primary keys, for
    -- backends that support this functionality. If 'Nothing', rows will be
    -- inserted one-at-a-time using 'connInsertSql'.
    , connUpsertSql :: Maybe (EntityDef -> NonEmpty (HaskellName,DBName) -> Text -> Text)
    -- ^ Some databases support performing UPSERT _and_ RETURN entity
    -- in a single call.
    --
    -- This field when set will be used to generate the UPSERT+RETURN sql given
    -- * an entity definition
    -- * updates to be run on unique key(s) collision
    --
    -- When left as 'Nothing', we find the unique key from entity def before
    -- * trying to fetch an entity by said key
    -- * perform an update when result found, else issue an insert
    -- * return new entity from db
    --
    -- @since 2.6
    , connPutManySql :: Maybe (EntityDef -> Int -> Text)
    -- ^ Some databases support performing bulk UPSERT, specifically
    -- "insert or replace many records" in a single call.
    --
    -- This field when set, given
    -- * an entity definition
    -- * number of records to be inserted
    -- should produce a PUT MANY sql with placeholders for records
    --
    -- When left as 'Nothing', we default to using 'defaultPutMany'.
    --
    -- @since 2.8.1
    , connStmtMap :: IORef (Map Text Statement)
    -- ^ A reference to the cache of statements. 'Statement's are keyed by
    -- the 'Text' queries that generated them.
    , connClose :: IO ()
    -- ^ Close the underlying connection.
    , connMigrateSql
        :: [EntityDef]
        -> (Text -> IO Statement)
        -> EntityDef
        -> IO (Either [Text] [(Bool, Text)])
    -- ^ This function returns the migrations required to include the
    -- 'EntityDef' parameter in the @['EntityDef']@ database. This might
    -- include creating a new table if the entity is not present, or
    -- altering an existing table if it is.
    , connBegin :: (Text -> IO Statement) -> Maybe IsolationLevel -> IO ()
    -- ^ A function to begin a transaction for the underlying database.
    , connCommit :: (Text -> IO Statement) -> IO ()
    -- ^ A function to commit a transaction to the underlying database.
    , connRollback :: (Text -> IO Statement) -> IO ()
    -- ^ A function to roll back a transaction on the underlying database.
    , connEscapeName :: DBName -> Text
    -- ^ A function to escape a name for the underlying database. MySQL
    -- uses backtick characters, while postgresql uses double quoes.
    , connNoLimit :: Text
    , connRDBMS :: Text
    -- ^ A tag displaying what database the 'SqlBackend' is for. Can be
    -- used to differentiate features in downstream libraries for different
    -- database backends.
    , connLimitOffset :: (Int,Int) -> Bool -> Text -> Text
    -- ^ Attach a 'LIMIT/OFFSET' clause to a SQL query. Note that
    -- LIMIT/OFFSET is problematic for performance, and indexed range
    -- queries are the superior way to offer pagination.
    , connLogFunc :: LogFunc
    -- ^ A log function for the 'SqlBackend' to use.
    , connMaxParams :: Maybe Int
    -- ^ Some databases (probably only Sqlite) have a limit on how
    -- many question-mark parameters may be used in a statement
    --
    -- @since 2.6.1
    , connRepsertManySql :: Maybe (EntityDef -> Int -> Text)
    -- ^ Some databases support performing bulk an atomic+bulk INSERT where
    -- constraint conflicting entities can replace existing entities.
    --
    -- This field when set, given
    -- * an entity definition
    -- * number of records to be inserted
    -- should produce a INSERT sql with placeholders for primary+record fields
    --
    -- When left as 'Nothing', we default to using 'defaultRepsertMany'.
    --
    -- @since 2.9.0
    }

instance HasPersistBackend SqlBackend where
    type BaseBackend SqlBackend = SqlBackend
    persistBackend = id

instance IsPersistBackend SqlBackend where
    mkPersistBackend = id

-- | An SQL backend which can only handle read queries
--
-- The constructor was exposed in 2.10.0.
newtype SqlReadBackend = SqlReadBackend { unSqlReadBackend :: SqlBackend } 

instance HasPersistBackend SqlReadBackend where
    type BaseBackend SqlReadBackend = SqlBackend
    persistBackend = unSqlReadBackend

instance IsPersistBackend SqlReadBackend where
    mkPersistBackend = SqlReadBackend

-- | An SQL backend which can handle read or write queries
--
-- The constructor was exposed in 2.10.0
newtype SqlWriteBackend = SqlWriteBackend { unSqlWriteBackend :: SqlBackend }

instance HasPersistBackend SqlWriteBackend where
    type BaseBackend SqlWriteBackend = SqlBackend
    persistBackend = unSqlWriteBackend

instance IsPersistBackend SqlWriteBackend where
    mkPersistBackend = SqlWriteBackend

-- | Useful for running a write query against an untagged backend with unknown capabilities.
writeToUnknown :: Monad m => ReaderT SqlWriteBackend m a -> ReaderT SqlBackend m a
writeToUnknown ma = do
  unknown <- ask
  lift . runReaderT ma $ SqlWriteBackend unknown

-- | Useful for running a read query against a backend with read and write capabilities.
readToWrite :: Monad m => ReaderT SqlReadBackend m a -> ReaderT SqlWriteBackend m a
readToWrite ma = do
  write <- ask
  lift . runReaderT ma . SqlReadBackend $ unSqlWriteBackend write

-- | Useful for running a read query against a backend with unknown capabilities.
readToUnknown :: Monad m => ReaderT SqlReadBackend m a -> ReaderT SqlBackend m a
readToUnknown ma = do
  unknown <- ask
  lift . runReaderT ma $ SqlReadBackend unknown

-- | A constraint synonym which witnesses that a backend is SQL and can run read queries.
type SqlBackendCanRead backend =
    ( BackendCompatible SqlBackend backend
    , PersistQueryRead backend, PersistStoreRead backend, PersistUniqueRead backend
    )

-- | A constraint synonym which witnesses that a backend is SQL and can run read and write queries.
type SqlBackendCanWrite backend =
    ( SqlBackendCanRead backend
    , PersistQueryWrite backend, PersistStoreWrite backend, PersistUniqueWrite backend
    )

-- | Like @SqlPersistT@ but compatible with any SQL backend which can handle read queries.
type SqlReadT m a = forall backend. (SqlBackendCanRead backend) => ReaderT backend m a

-- | Like @SqlPersistT@ but compatible with any SQL backend which can handle read and write queries.
type SqlWriteT m a = forall backend. (SqlBackendCanWrite backend) => ReaderT backend m a

-- | A backend which is a wrapper around @SqlBackend@.
type IsSqlBackend backend = (IsPersistBackend backend, BaseBackend backend ~ SqlBackend)
