{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.Persist.Sql.Types.Internal
    ( HasPersistBackend (..)
    , IsPersistBackend (..)
    , SqlReadBackend (unSqlReadBackend)
    , SqlWriteBackend (unSqlWriteBackend)
    , readToUnknown
    , readToWrite
    , writeToUnknown
    , LogFunc
    , InsertSqlResult (..)
    , Statement (..)
    , SqlBackend (..)
    , SqlBackendCanRead
    , SqlBackendCanWrite
    , SqlReadT
    , SqlWriteT
    , IsSqlBackend
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LogSource, LogLevel)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Acquire (Acquire)
import Data.Conduit (Source)
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.Class
  ( HasPersistBackend (..)
  , PersistQueryRead, PersistQueryWrite
  , PersistStoreRead, PersistStoreWrite
  , PersistUniqueRead, PersistUniqueWrite
  )
import Database.Persist.Class.PersistStore (IsPersistBackend (..))
import Database.Persist.Types
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr)

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
                -> Acquire (Source m [PersistValue])
    }

data SqlBackend db = SqlBackend
    { connPrepare :: Text -> IO Statement
    -- | table name, column names, id name, either 1 or 2 statements to run
    , connInsertSql :: EntityDef -> [PersistValue] -> InsertSqlResult
    , connInsertManySql :: Maybe (EntityDef -> [[PersistValue]] -> InsertSqlResult) -- ^ SQL for inserting many rows and returning their primary keys, for backends that support this functioanlity. If 'Nothing', rows will be inserted one-at-a-time using 'connInsertSql'.
    , connUpsertSql :: Maybe (EntityDef -> Text -> Text)
    , connStmtMap :: IORef (Map Text Statement)
    , connClose :: IO ()
    , connMigrateSql
        :: [EntityDef]
        -> (Text -> IO Statement)
        -> EntityDef
        -> IO (Either [Text] [(Bool, Text)])
    , connBegin :: (Text -> IO Statement) -> IO ()
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
    }
    deriving Typeable
instance HasPersistBackend (SqlBackend db) where
    type BaseBackend (SqlBackend db) = SqlBackend db
    persistBackend = id
instance IsPersistBackend (SqlBackend db) where
    mkPersistBackend = id

-- | An SQL backend which can only handle read queries
newtype SqlReadBackend db = SqlReadBackend { unSqlReadBackend :: SqlBackend db } deriving Typeable
instance HasPersistBackend (SqlReadBackend db) where
    type BaseBackend (SqlReadBackend db) = SqlBackend db
    persistBackend = unSqlReadBackend
instance IsPersistBackend (SqlReadBackend db) where
    mkPersistBackend = SqlReadBackend

-- | An SQL backend which can handle read or write queries
newtype SqlWriteBackend db = SqlWriteBackend { unSqlWriteBackend :: SqlBackend db } deriving Typeable
instance HasPersistBackend (SqlWriteBackend db) where
    type BaseBackend (SqlWriteBackend db) = SqlBackend db
    persistBackend = unSqlWriteBackend
instance IsPersistBackend (SqlWriteBackend db) where
    mkPersistBackend = SqlWriteBackend

-- | Useful for running a write query against an untagged backend with unknown capabilities.
writeToUnknown :: Monad m => ReaderT (SqlWriteBackend db) m a -> ReaderT (SqlBackend db) m a
writeToUnknown ma = do
  unknown <- ask
  lift . runReaderT ma $ SqlWriteBackend unknown

-- | Useful for running a read query against a backend with read and write capabilities.
readToWrite :: Monad m => ReaderT (SqlReadBackend db) m a -> ReaderT (SqlWriteBackend db) m a
readToWrite ma = do
  write <- ask
  lift . runReaderT ma . SqlReadBackend $ unSqlWriteBackend write

-- | Useful for running a read query against a backend with unknown capabilities.
readToUnknown :: Monad m => ReaderT (SqlReadBackend db) m a -> ReaderT (SqlBackend db) m a
readToUnknown ma = do
  unknown <- ask
  lift . runReaderT ma $ SqlReadBackend unknown

-- | A constraint synonym which witnesses that a backend is SQL and can run read queries.
type SqlBackendCanRead backend db =
  ( IsSqlBackend backend db
  , PersistQueryRead backend, PersistStoreRead backend, PersistUniqueRead backend
  )
-- | A constraint synonym which witnesses that a backend is SQL and can run read and write queries.
type SqlBackendCanWrite backend db =
  ( SqlBackendCanRead backend db
  , PersistQueryWrite backend, PersistStoreWrite backend, PersistUniqueWrite backend
  )
-- | Like @SqlPersistT@ but compatible with any SQL backend which can handle read queries.
type SqlReadT m a = forall backend db. (SqlBackendCanRead backend db) => ReaderT backend m a
-- | Like @SqlPersistT@ but compatible with any SQL backend which can handle read and write queries.
type SqlWriteT m a = forall backend db. (SqlBackendCanWrite backend db) => ReaderT backend m a
-- | A backend which is a wrapper around @SqlBackend@.
type IsSqlBackend backend db = (IsPersistBackend backend, BaseBackend backend ~ SqlBackend db)
