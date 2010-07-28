{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
-- | A sqlite backend for persistent.
module Database.Persist.Sqlite
    ( SqliteReader
    , runSqlite
    , runSqliteConn
    , withSqlite
    , withSqliteConn
    , Connection (..)
    , Pool
    , module Database.Persist
    , initialize
    ) where

import Database.Persist
import Database.Persist.Base
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Database.Sqlite hiding (Connection)
import qualified Database.Sqlite as Sqlite
import qualified Database.Persist.GenericSql as G
import Control.Applicative (Applicative)
import Data.Int (Int64)
import Database.Persist.Pool
import Data.IORef
import qualified Data.Map as Map

type StmtMap = Map.Map String Statement
data Connection = Connection Sqlite.Connection (IORef StmtMap)

-- | A ReaderT monad transformer holding a sqlite database connection.
newtype SqliteReader m a = SqliteReader (ReaderT Connection m a)
    deriving (Monad, MonadIO, MonadTrans, MonadCatchIO, Functor,
              Applicative)

-- | Handles opening and closing of the database connection pool automatically.
withSqlite :: MonadCatchIO m
           => String
           -> Int -- ^ number of connections to open
           -> (Pool Connection -> m a) -> m a
withSqlite s i f = createPool (open' s) close' i f

open' :: String -> IO Connection
open' s = do
    conn <- open s
    stmtmap <- newIORef $ Map.empty
    return $ Connection conn stmtmap

close' :: Connection -> IO ()
close' (Connection conn istmtmap) = do
    stmtmap <- readIORef istmtmap
    mapM_ finalize (Map.elems stmtmap)
    close conn

-- | Handles opening and closing of the database connection automatically.
-- You probably want to take advantage of connection pooling by using
-- withSqlite instead.
withSqliteConn :: MonadCatchIO m => String -> (Connection -> m a) -> m a
withSqliteConn s f = bracket (liftIO $ open' s) (liftIO . close') f

-- | Run a series of database actions within a single transactions.
-- On any exception, the transaction is rolled back.
runSqlite :: MonadCatchIO m => SqliteReader m a -> Pool Connection -> m a
runSqlite r pconn = withPool' pconn $ runSqliteConn r

-- | Run a series of database actions within a single transactions.
-- On any exception, the transaction is rolled back.
-- You probably want to take advantage of connection pooling by using runSqlite instead
runSqliteConn :: MonadCatchIO m => SqliteReader m a -> Connection -> m a
runSqliteConn (SqliteReader r) conn = do
    beginS <- liftIO $ getStmt "BEGIN" conn
    commitS <- liftIO $ getStmt "COMMIT" conn
    rollbackS <- liftIO $ getStmt "ROLLBACK" conn
    Done <- liftIO $ step beginS
    res <- onException (runReaderT r conn) $ liftIO (resetAll conn >> step rollbackS)
    Done <- liftIO $ step commitS
    return res

-- | Reset all statements; necessary to call a ROLLBACK.
resetAll :: Connection -> IO ()
resetAll (Connection conn imap) = do
    stmts <- readIORef imap
    mapM_ reset $ Map.elems stmts

getStmt :: String -> Connection -> IO Statement
getStmt sql (Connection conn istmtmap) = do
    stmtmap <- readIORef istmtmap
    case Map.lookup sql stmtmap of
        Just stmt -> do
            reset stmt
            return stmt
        Nothing -> do
            stmt <- prepare conn sql
            let stmtmap' = Map.insert sql stmt stmtmap
            writeIORef istmtmap stmtmap'
            return stmt

withStmt :: MonadCatchIO m
         => String
         -> [PersistValue]
         -> (G.RowPopper (SqliteReader m) -> SqliteReader m a)
         -> SqliteReader m a
withStmt sql vals f = do
    conn <- SqliteReader ask
    stmt <- liftIO $ getStmt sql conn -- FIXME do we need a reset here?
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
    stmt <- liftIO $ getStmt sql conn
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
genericSql = G.GenericSql withStmt execute insert' tableExists
                          "INTEGER PRIMARY KEY" showSqlType

initialize :: (MonadCatchIO m, PersistEntity v) => v -> SqliteReader m ()
initialize = G.initialize genericSql -- FIXME

instance MonadCatchIO m => PersistBackend (SqliteReader m) where
    insert = G.insert genericSql
    get = G.get genericSql
    replace = G.replace genericSql
    select = G.select genericSql
    count = G.count genericSql
    deleteWhere = G.deleteWhere genericSql
    update = G.update genericSql
    updateWhere = G.updateWhere genericSql
    getBy = G.getBy genericSql
    delete = G.delete genericSql
    deleteBy = G.deleteBy genericSql

showSqlType :: SqlType -> String
showSqlType SqlString = "VARCHAR"
showSqlType SqlInteger = "INTEGER"
showSqlType SqlReal = "REAL"
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlBlob = "BLOB"
showSqlType SqlBool = "BOOLEAN"
