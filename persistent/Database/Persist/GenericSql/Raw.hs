{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Persist.GenericSql.Raw
    ( withStmt
    , execute
    , SqlPersist (..)
    , getStmt'
    , getStmt
    ) where

import qualified Database.Persist.GenericSql.Internal as I
import Database.Persist.GenericSql.Internal hiding (execute, withStmt)
import Database.Persist.Store (PersistValue)
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Control (MonadBaseControl (..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl (..))
import Control.Monad (liftM)
#define MBCIO MonadBaseControl IO
import Data.Text (Text)
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Resource (MonadResource (..))
import Data.Conduit
import System.Log.FastLogger (MonadLogging (..))

newtype SqlPersist m a = SqlPersist { unSqlPersist :: ReaderT Connection m a }
    deriving (Monad, MonadIO, MonadTrans, Functor, Applicative, MonadPlus)

instance MonadThrow m => MonadThrow (SqlPersist m) where
    monadThrow = lift . monadThrow

instance MonadBase b m => MonadBase b (SqlPersist m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (SqlPersist m) where
     newtype StM (SqlPersist m) a = StMSP {unStMSP :: ComposeSt SqlPersist m a}
     liftBaseWith = defaultLiftBaseWith StMSP
     restoreM     = defaultRestoreM   unStMSP
instance MonadTransControl SqlPersist where
    newtype StT SqlPersist a = StReader {unStReader :: a}
    liftWith f = SqlPersist $ ReaderT $ \r -> f $ \t -> liftM StReader $ runReaderT (unSqlPersist t) r
    restoreT = SqlPersist . ReaderT . const . liftM unStReader

instance MonadResource m => MonadResource (SqlPersist m) where
    register = lift . register
    release = lift . release
    allocate a = lift . allocate a
    resourceMask = lift . resourceMask

class (MonadIO m, MonadLogging m) => MonadSqlPersist m where
    askSqlConn :: m Connection

instance (MonadIO m, MonadLogging m) => MonadSqlPersist (SqlPersist m) where
    askSqlConn = SqlPersist ask
instance MonadSqlPersist m => MonadSqlPersist (ResourceT m) where
    askSqlConn = lift askSqlConn
-- FIXME add a bunch of MonadSqlPersist instances for all transformers

instance MonadLogging m => MonadLogging (SqlPersist m) where
    monadLoggingLog a b c = lift $ monadLoggingLog a b c

withStmt :: (MonadSqlPersist m, MonadResource m)
         => Text
         -> [PersistValue]
         -> Source m [PersistValue]
withStmt sql vals = do
    lift $ $logSQL sql vals
    conn <- lift askSqlConn
    bracketP
        (getStmt' conn sql)
        I.reset
        (flip I.withStmt vals)

execute :: MonadSqlPersist m => Text -> [PersistValue] -> m ()
execute sql vals = do
    $logSQL sql vals
    stmt <- getStmt sql
    liftIO $ I.execute stmt vals
    liftIO $ reset stmt

getStmt :: MonadSqlPersist m => Text -> m Statement
getStmt sql = do
    conn <- askSqlConn
    liftIO $ getStmt' conn sql

getStmt' :: Connection -> Text -> IO Statement
getStmt' conn sql = do
    smap <- liftIO $ readIORef $ stmtMap conn
    case Map.lookup sql smap of
        Just stmt -> return stmt
        Nothing -> do
            stmt <- liftIO $ prepare conn sql
            liftIO $ writeIORef (stmtMap conn) $ Map.insert sql stmt smap
            return stmt
