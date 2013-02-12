{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Persist.GenericSql.Raw
    ( withStmt
    , execute
    , executeCount
    , SqlPersist (..)
    , getStmt'
    , getStmt
    , SqlBackend
    , MonadSqlPersist (..)
    , StatementAlreadyFinalized (..)
    ) where

import qualified Database.Persist.GenericSql.Internal as I
import Database.Persist.GenericSql.Internal hiding (execute, withStmt)
import Database.Persist.Store (PersistValue)
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Logger (logDebugS)
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Control (MonadBaseControl (..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl (..))
import Control.Monad (liftM, when)
import Control.Exception (throwIO, Exception)
#define MBCIO MonadBaseControl IO
import Data.Text (Text, pack)
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Resource (MonadResource (..))
import Data.Conduit
import Control.Monad.Logger (MonadLogger (..))
import Data.Monoid (Monoid)
import Data.Typeable (Typeable)
import Data.Int (Int64)

import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

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
#if MIN_VERSION_resourcet(0,4,0)
    liftResourceT = lift . liftResourceT
#else
    register = lift . register
    release = lift . release
    allocate a = lift . allocate a
    resourceMask = lift . resourceMask
#endif

class (MonadIO m, MonadLogger m) => MonadSqlPersist m where
    askSqlConn :: m Connection

instance (MonadIO m, MonadLogger m) => MonadSqlPersist (SqlPersist m) where
    askSqlConn = SqlPersist ask

#define GO(T) instance (MonadSqlPersist m) => MonadSqlPersist (T m) where askSqlConn = lift askSqlConn
#define GOX(X, T) instance (X, MonadSqlPersist m) => MonadSqlPersist (T m) where askSqlConn = lift askSqlConn
GO(LoggingT)
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GO(ResourceT)
GO(Pipe l i o u)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX

instance MonadLogger m => MonadLogger (SqlPersist m) where
    monadLoggerLog a b c = lift $ monadLoggerLog a b c
    monadLoggerLogSource a b c = lift . monadLoggerLogSource a b c

withStmt :: (MonadSqlPersist m, MonadResource m)
         => Text
         -> [PersistValue]
         -> Source m [PersistValue]
withStmt sql vals = do
    lift $ $logDebugS (pack "SQL") $ pack $ show sql ++ " " ++ show vals
    conn <- lift askSqlConn
    bracketP
        (getStmt' conn sql)
        I.reset
        (flip I.withStmt vals)

execute :: MonadSqlPersist m => Text -> [PersistValue] -> m ()
execute x y = liftM (const ()) $ executeCount x y

executeCount :: MonadSqlPersist m => Text -> [PersistValue] -> m Int64
executeCount sql vals = do
    $logDebugS (pack "SQL") $ pack $ show sql ++ " " ++ show vals
    stmt <- getStmt sql
    res <- liftIO $ I.execute stmt vals
    liftIO $ reset stmt
    return res

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
            stmt' <- liftIO $ prepare conn sql
            iactive <- liftIO $ newIORef True
            let stmt = I.Statement
                    { finalize = do
                        active <- readIORef iactive
                        if active
                            then do
                                finalize stmt'
                                writeIORef iactive False
                            else return ()
                    , reset = do
                        active <- readIORef iactive
                        when active $ reset stmt'
                    , I.execute = \x -> do
                        active <- readIORef iactive
                        if active
                            then I.execute stmt' x
                            else throwIO $ StatementAlreadyFinalized sql
                    , I.withStmt = \x -> do
                        active <- liftIO $ readIORef iactive
                        if active
                            then I.withStmt stmt' x
                            else liftIO $ throwIO $ StatementAlreadyFinalized sql
                    }
            liftIO $ writeIORef (stmtMap conn) $ Map.insert sql stmt smap
            return stmt

data StatementAlreadyFinalized = StatementAlreadyFinalized Text
    deriving (Typeable, Show)
instance Exception StatementAlreadyFinalized
