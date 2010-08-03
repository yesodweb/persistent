{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import "MonadCatchIO-transformers" Control.Monad.CatchIO

newtype SqlPersist m a = SqlPersist (ReaderT Connection m a)
    deriving (Monad, MonadIO, MonadCatchIO, MonadTrans, Functor, Applicative)

withStmt :: MonadCatchIO m => String -> [PersistValue]
         -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt sql vals pop = do
    stmt <- getStmt sql
    ret <- I.withStmt stmt vals pop
    liftIO $ reset stmt
    return ret

execute :: MonadIO m => String -> [PersistValue] -> SqlPersist m ()
execute sql vals = do
    stmt <- getStmt sql
    liftIO $ I.execute stmt vals
    liftIO $ reset stmt

getStmt :: MonadIO m => String -> SqlPersist m Statement
getStmt sql = do
    conn <- SqlPersist ask
    liftIO $ getStmt' conn sql

getStmt' :: Connection -> String -> IO Statement
getStmt' conn sql = do
    smap <- liftIO $ readIORef $ stmtMap conn
    case Map.lookup sql smap of
        Just stmt -> return stmt
        Nothing -> do
            stmt <- liftIO $ prepare conn sql
            liftIO $ writeIORef (stmtMap conn) $ Map.insert sql stmt smap
            return stmt
