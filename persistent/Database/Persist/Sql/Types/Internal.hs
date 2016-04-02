{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.Persist.Sql.Types.Internal
    ( HasPersistBackend (..)
    , IsPersistBackend (..)
    , SqlReadBackend (unReadSqlBackend)
    , SqlWriteBackend (unWriteSqlBackend)
    , readToUnknown
    , readToWrite
    , writeToUnknown
    , LogFunc
    , InsertSqlResult (..)
    , Statement (..)
    , SqlBackend (..)
    , CanWrite
    , ReadSqlBackend
    , WriteSqlBackend
    , SqlReadT
    , SqlWriteT
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

data SqlBackend = SqlBackend
    { connPrepare :: Text -> IO Statement
    -- | table name, column names, id name, either 1 or 2 statements to run
    , connInsertSql :: EntityDef -> [PersistValue] -> InsertSqlResult
    , connInsertManySql :: Maybe (EntityDef -> [[PersistValue]] -> InsertSqlResult) -- ^ SQL for inserting many rows and returning their primary keys, for backends that support this functioanlity. If 'Nothing', rows will be inserted one-at-a-time using 'connInsertSql'.
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
    }
    deriving Typeable
instance HasPersistBackend SqlBackend where
    type BaseBackend SqlBackend = SqlBackend
    persistBackend = id
instance IsPersistBackend SqlBackend where
    mkPersistBackend = id

newtype SqlReadBackend = SqlReadBackend { unReadSqlBackend :: SqlBackend } deriving Typeable
instance HasPersistBackend SqlReadBackend where
    type BaseBackend SqlReadBackend = SqlBackend
    persistBackend = unReadSqlBackend
instance IsPersistBackend SqlReadBackend where
    mkPersistBackend = SqlReadBackend

class CanWrite backend
instance CanWrite SqlWriteBackend
instance CanWrite SqlBackend

newtype SqlWriteBackend = SqlWriteBackend { unWriteSqlBackend :: SqlBackend } deriving Typeable
instance HasPersistBackend SqlWriteBackend where
    type BaseBackend SqlWriteBackend = SqlBackend
    persistBackend = unWriteSqlBackend
instance IsPersistBackend SqlWriteBackend where
    mkPersistBackend = SqlWriteBackend

writeToUnknown :: Monad m => ReaderT SqlWriteBackend m a -> ReaderT SqlBackend m a
writeToUnknown ma = do
  unknown <- ask
  lift . runReaderT ma $ SqlWriteBackend unknown

readToWrite :: Monad m => ReaderT SqlReadBackend m a -> ReaderT SqlWriteBackend m a
readToWrite ma = do
  write <- ask
  lift . runReaderT ma . SqlReadBackend $ unWriteSqlBackend write

readToUnknown :: Monad m => ReaderT SqlReadBackend m a -> ReaderT SqlBackend m a
readToUnknown ma = do
  unknown <- ask
  lift . runReaderT ma $ SqlReadBackend unknown

type ReadSqlBackend backend =
  ( BaseBackend backend ~ SqlBackend, IsPersistBackend backend
  , PersistQueryRead backend, PersistStoreRead backend, PersistUniqueRead backend
  )
type WriteSqlBackend backend =
  ( ReadSqlBackend backend, CanWrite backend
  , PersistQueryWrite backend, PersistStoreWrite backend, PersistUniqueWrite backend
  )
type SqlReadT m a = forall backend. (ReadSqlBackend backend) => ReaderT backend m a
type SqlWriteT m a = forall backend. (WriteSqlBackend backend) => ReaderT backend m a
