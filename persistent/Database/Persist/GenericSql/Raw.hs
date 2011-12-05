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
import Database.Persist.Base (PersistValue)
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Base (MonadBase (liftBase))
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl (..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl (..))
import Control.Monad (liftM)
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
#define MBCIO MonadControlIO
#endif
import Data.Text (Text)
import Control.Monad (MonadPlus)

newtype SqlPersist m a = SqlPersist { unSqlPersist :: ReaderT Connection m a }
    deriving (Monad, MonadIO, MonadTrans, Functor, Applicative, MonadPlus
#if !MIN_VERSION_monad_control(0, 3, 0)
      , MonadControlIO
#endif
      )

instance MonadBase b m => MonadBase b (SqlPersist m) where
    liftBase = lift . liftBase

#if MIN_VERSION_monad_control(0, 3, 0)
instance MonadBaseControl b m => MonadBaseControl b (SqlPersist m) where
     newtype StM (SqlPersist m) a = StMSP {unStMSP :: ComposeSt SqlPersist m a}
     liftBaseWith = defaultLiftBaseWith StMSP
     restoreM     = defaultRestoreM   unStMSP
instance MonadTransControl SqlPersist where
    newtype StT SqlPersist a = StReader {unStReader :: a}
    liftWith f = SqlPersist $ ReaderT $ \r -> f $ \t -> liftM StReader $ runReaderT (unSqlPersist t) r
    restoreT = SqlPersist . ReaderT . const . liftM unStReader
#endif

withStmt :: (MonadIO m, MBCIO m) => Text -> [PersistValue]
         -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt sql vals pop = do
    stmt <- getStmt sql
    ret <- I.withStmt stmt vals pop
    liftIO $ reset stmt
    return ret

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
