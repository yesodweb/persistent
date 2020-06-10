{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Control.Monad.Logger (LogSource, LogLevel)
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
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Loc)
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

data SqlBackend = SqlBackend
    { connPrepare :: Text -> IO Statement
    -- | table name, column names, id name, either 1 or 2 statements to run
    , connInsertSql :: EntityDef -> [PersistValue] -> InsertSqlResult
    , connInsertManySql :: Maybe (EntityDef -> [[PersistValue]] -> InsertSqlResult)
    -- ^ SQL for inserting many rows and returning their primary keys, for
    -- backends that support this functionality. If 'Nothing', rows will be
    -- inserted one-at-a-time using 'connInsertSql'.
    , connUpsertSql :: Maybe (EntityDef -> NonEmpty UniqueDef -> Text -> Text)
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
    , connClose :: IO ()
    , connMigrateSql
        :: [EntityDef]
        -> (Text -> IO Statement)
        -> EntityDef
        -> IO (Either [Text] [(Bool, Text)])
    , connBegin :: (Text -> IO Statement) -> Maybe IsolationLevel -> IO ()
    , connCommit :: (Text -> IO Statement) -> IO ()
    , connRollback :: (Text -> IO Statement) -> IO ()
    , connEscapeName :: DBName -> Text
    , connNoLimit :: Text
    , connRDBMS :: Text
    , connLimitOffset :: (Int,Int) -> Bool -> Text -> Text
    , connLogFunc :: LogFunc
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
    deriving Typeable
instance HasPersistBackend SqlBackend where
    type BaseBackend SqlBackend = SqlBackend
    persistBackend = id
instance IsPersistBackend SqlBackend where
    mkPersistBackend = id

-- | An SQL backend which can only handle read queries
--
-- The constructor was exposed in 2.10.0.
newtype SqlReadBackend = SqlReadBackend { unSqlReadBackend :: SqlBackend } deriving Typeable
instance HasPersistBackend SqlReadBackend where
    type BaseBackend SqlReadBackend = SqlBackend
    persistBackend = unSqlReadBackend
instance IsPersistBackend SqlReadBackend where
    mkPersistBackend = SqlReadBackend

-- | An SQL backend which can handle read or write queries
--
-- The constructor was exposed in 2.10.0
newtype SqlWriteBackend = SqlWriteBackend { unSqlWriteBackend :: SqlBackend } deriving Typeable
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
