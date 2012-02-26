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
import Control.Monad.Trans.Resource (MonadThrow (..), MonadResource (..))
import qualified Data.Conduit as C

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

withStmt :: MonadResource m
         => Text
         -> [PersistValue]
         -> C.Source (SqlPersist m) [PersistValue]
withStmt sql vals = C.Source
    { C.sourcePull = do
        stmt <- getStmt sql
        let src = I.withStmt stmt vals
        pull stmt src
    , C.sourceClose = return ()
    }
  where
    pull stmt src = do
        res <- C.sourcePull src
        case res of
            C.Closed -> do
                liftIO $ I.reset stmt
                return C.Closed
            C.Open src' val -> return $ C.Open
                (C.Source (pull stmt src') (close' stmt src'))
                val
    close' stmt src = do
        liftIO $ I.reset stmt
        C.sourceClose src

execute :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute sql vals = do
    stmt <- getStmt sql
    liftIO $ I.execute stmt vals
    liftIO $ reset stmt

getStmt :: MonadIO m => Text -> SqlPersist m Statement
getStmt sql = do
    conn <- SqlPersist ask
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
