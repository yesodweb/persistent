{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
-- | A sqlite backend for persistent.
module Database.Persist.Sqlite
    ( SqliteReader
    , persistSqlite
    , runSqlite
    , withSqlite
      -- * Re-exports
    , Database
    , open
    , close
    , Int64
    , module Database.Persist.Helper
    , persist
    ) where

import Database.Persist (PersistValue (..), PersistBackend (..))
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Database.Sqlite
import Database.Persist.Quasi
import qualified Database.Persist.GenericSql as G
import Control.Applicative (Applicative)
import Data.Int (Int64)

-- | Generate data types and instances for the given entity definitions. Can
-- deal directly with the output of the 'persist' quasi-quoter.
persistSqlite :: [EntityDef] -> Q [Dec]
persistSqlite = fmap concat . mapM mkEntity

-- | A ReaderT monad transformer holding a sqlite database connection.
newtype SqliteReader m a = SqliteReader (ReaderT Database m a)
    deriving (Monad, MonadIO, MonadTrans, MonadCatchIO, Functor,
              Applicative)

-- | Handles opening and closing of the database connection automatically.
withSqlite :: MonadCatchIO m => String -> (Database -> m a) -> m a
withSqlite s f = bracket (liftIO $ open s) (liftIO . close) f

-- | Run a series of database actions within a single transactions. On any
-- exception, the transaction is rolled back.
runSqlite :: MonadCatchIO m => SqliteReader m a -> Database -> m a
runSqlite (SqliteReader r) conn = do
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
         -> (G.RowPopper (SqliteReader m) -> SqliteReader m a)
         -> SqliteReader m a
withStmt sql vals f = do
    conn <- SqliteReader ask
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
    conn <- SqliteReader ask
    bracket (liftIO $ prepare conn sql) (liftIO . finalize) $ \stmt -> do
        liftIO $ bind stmt vals
        Done <- liftIO $ step stmt
        return ()

insert' :: MonadCatchIO m
        => String -> [String] -> [PersistValue] -> SqliteReader m Int64
insert' t cols vals = do
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

genericSql :: MonadCatchIO m => G.GenericSql (SqliteReader m)
genericSql = G.GenericSql withStmt execute insert' tableExists "INTEGER PRIMARY KEY"

instance MonadCatchIO m => PersistBackend (SqliteReader m) where
    initialize = G.initialize genericSql
    insert = G.insert genericSql
    get = G.get genericSql
    replace = G.replace genericSql
    select = G.select genericSql
    deleteWhere = G.deleteWhere genericSql
    update = G.update genericSql
    updateWhere = G.updateWhere genericSql
    getBy = G.getBy genericSql
    delete = G.delete genericSql
    deleteBy = G.deleteBy genericSql
