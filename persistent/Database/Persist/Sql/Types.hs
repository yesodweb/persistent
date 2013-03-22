{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Persist.Sql.Types where

import Control.Exception (Exception)
import Control.Monad.Trans.Resource (MonadResource (..), MonadThrow (..))
import Control.Monad.Logger (MonadLogger (..))
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Applicative (Applicative (..))
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Base (MonadBase (..))
import Control.Monad (MonadPlus (..))
import Data.Typeable (Typeable)
import Control.Monad (liftM)
import Database.Persist.Types
import Data.Text (Text)
import Data.IORef (IORef)
import Data.Map (Map)
import Database.Persist.Class.PersistEntity (PersistEntity)
import Data.Int (Int64)
import Data.Conduit (Source)
import Data.Conduit.Pool (Pool)

data InsertSqlResult = ISRSingle Text
                     | ISRInsertGet Text Text

data Connection = Connection
    { prepare :: Text -> IO Statement
    -- ^ table name, column names, id name, either 1 or 2 statements to run
    , insertSql :: DBName -> [DBName] -> DBName -> InsertSqlResult
    , stmtMap :: IORef (Map Text Statement)
    , close :: IO ()
    , migrateSql :: forall v. PersistEntity v
                 => [EntityDef]
                 -> (Text -> IO Statement)
                 -> v
                 -> IO (Either [Text] [(Bool, Text)])
    , begin :: (Text -> IO Statement) -> IO ()
    , commitC :: (Text -> IO Statement) -> IO ()
    , rollbackC :: (Text -> IO Statement) -> IO ()
    , escapeName :: DBName -> Text
    , noLimit :: Text
    }
data Statement = Statement
    { finalize :: IO ()
    , reset :: IO ()
    , execute :: [PersistValue] -> IO Int64
    , withStmt :: forall m. MonadResource m
               => [PersistValue]
               -> Source m [PersistValue]
    }

data Column = Column
    { cName      :: DBName
    , cNull      :: Bool
    , cType      :: SqlType
    , cDefault   :: Maybe Text
    , cMaxLen    :: Maybe Integer
    , cReference :: (Maybe (DBName, DBName)) -- table name, constraint name
    }
    deriving (Eq, Ord, Show)

data StatementAlreadyFinalized = StatementAlreadyFinalized Text
    deriving (Typeable, Show)
instance Exception StatementAlreadyFinalized

data SqlBackend

newtype SqlPersist m a = SqlPersist { unSqlPersist :: ReaderT Connection m a }
    deriving (Monad, MonadIO, MonadTrans, Functor, Applicative, MonadPlus)

instance MonadThrow m => MonadThrow (SqlPersist m) where
    monadThrow = lift . monadThrow

instance MonadBase backend m => MonadBase backend (SqlPersist m) where
    liftBase = lift . liftBase

instance MonadBaseControl backend m => MonadBaseControl backend (SqlPersist m) where
     newtype StM (SqlPersist m) a = StMSP {unStMSP :: ComposeSt SqlPersist m a}
     liftBaseWith = defaultLiftBaseWith StMSP
     restoreM     = defaultRestoreM   unStMSP
instance MonadTransControl SqlPersist where
    newtype StT SqlPersist a = StReader {unStReader :: a}
    liftWith f = SqlPersist $ ReaderT $ \r -> f $ \t -> liftM StReader $ runReaderT (unSqlPersist t) r
    restoreT = SqlPersist . ReaderT . const . liftM unStReader

instance MonadResource m => MonadResource (SqlPersist m) where
    liftResourceT = lift . liftResourceT

instance MonadLogger m => MonadLogger (SqlPersist m) where
    monadLoggerLog a b c = lift . monadLoggerLog a b c

type Sql = Text

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]

type Migration m = WriterT [Text] (WriterT CautiousMigration m) ()

type ConnectionPool = Pool Connection
