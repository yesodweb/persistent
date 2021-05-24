{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# language RecordWildCards #-}
{-# language DuplicateRecordFields #-}

-- | Breaking changes to this module are not reflected in the major version
-- number. Prefer to import from "Database.Persist.Sql" instead. If you neeed
-- something from this module, please file an issue on GitHub.
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

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import Database.Persist.Class
    ( HasPersistBackend (..)
    , PersistQueryRead, PersistQueryWrite
    , PersistStoreRead, PersistStoreWrite
    , PersistUniqueRead, PersistUniqueWrite
    , BackendCompatible(..)
    )
import Database.Persist.Class.PersistStore (IsPersistBackend (..))
import Database.Persist.SqlBackend.Internal
import Database.Persist.SqlBackend.Internal.InsertSqlResult
import Database.Persist.SqlBackend.Internal.MkSqlBackend
import Database.Persist.SqlBackend.Internal.Statement
import Database.Persist.SqlBackend.Internal.IsolationLevel

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
type IsSqlBackend backend =
    ( IsPersistBackend backend
    , BaseBackend backend ~ SqlBackend
    )
