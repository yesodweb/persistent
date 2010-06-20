{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sqlite
    ( SqliteReader
    , Database
    , runSqlite
    , open
    , close
    , withSqlite
    , persistSqlite
    , Int64
    , module Database.Persist.Helper
    , persist
    ) where

import Database.Persist (PersistValue (..))
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Database.Sqlite
import Database.Persist.Quasi
import Database.Persist.GenericSql

persistSqlite :: [EntityDef] -> Q [Dec]
persistSqlite = fmap concat . mapM derivePersistSqliteReader

type SqliteReader = ReaderT Database

withSqlite :: MonadCatchIO m => String -> (Database -> m a) -> m a
withSqlite s f = bracket (liftIO $ open s) (liftIO . close) f

runSqlite :: MonadCatchIO m => SqliteReader m a -> Database -> m a
runSqlite r conn = do
    Done <- liftIO begin
    res <- onException (runReaderT r conn) $ liftIO rollback
    Done <- liftIO commit
    return res
  where
    begin = bracket (prepare conn "BEGIN") finalize step
    commit = bracket (prepare conn "COMMIT") finalize step
    rollback = bracket (prepare conn "ROLLBACK") finalize step

withStmt :: MonadCatchIO m
         => String
         -> [PersistValue]
         -> (RowPopper (SqliteReader m) -> SqliteReader m a)
         -> SqliteReader m a
withStmt sql vals f = do
    conn <- ask
    bracket (liftIO $ prepare conn sql) (liftIO . finalize) $ \stmt -> do
        liftIO $ bind stmt vals
        f $ go stmt
  where
    go stmt = liftIO $ do
        x <- step stmt
        case x of
            Done -> return Nothing
            Row -> do
                cols <- liftIO $ columns stmt
                return $ Just cols

execute :: MonadCatchIO m => String -> [PersistValue] -> SqliteReader m ()
execute sql vals = do
    conn <- ask
    bracket (liftIO $ prepare conn sql) (liftIO . finalize) $ \stmt -> do
        liftIO $ bind stmt vals
        Done <- liftIO $ step stmt
        return ()

insert :: MonadCatchIO m
       => String -> [String] -> [PersistValue] -> SqliteReader m Int64
insert t cols vals = do
    let sql = "INSERT INTO " ++ t ++
              "(" ++ intercalate "," cols ++ ") VALUES(" ++
              intercalate "," (map (const "?") cols) ++ ")"
    execute sql vals
    withStmt "SELECT last_insert_rowid()" [] $ \pop -> do
        Just [PersistInt64 i] <- pop
        return i

tableExists :: MonadCatchIO m => String -> SqliteReader m Bool
tableExists t = withStmt sql [PersistString t] $ \pop -> do
    Just [PersistInt64 i] <- pop
    return $ i == 1
  where
    sql = "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name=?"

derivePersistSqliteReader :: EntityDef -> Q [Dec]
derivePersistSqliteReader t = do
    let wrap = ConT ''ReaderT `AppT` ConT ''Database
    gs <- [|GenericSql withStmt execute insert tableExists "INTEGER PRIMARY KEY"|]
    deriveGenericSql wrap gs t
