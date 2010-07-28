{-# LANGUAGE PackageImports #-}
-- | A sqlite backend for persistent.
module Database.Persist.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , module Database.Persist
    , module Database.Persist.GenericSql
    ) where

import Database.Persist
import Database.Persist.Base
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal

import qualified Database.Sqlite as Sqlite

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Data.IORef
import qualified Data.Map as Map
import Data.Char (toLower)

withSqlitePool :: MonadCatchIO m
               => String
               -> Int -- ^ number of connections to open
               -> (ConnectionPool -> m a) -> m a
withSqlitePool s = withSqlPool $ open' s

withSqliteConn :: MonadCatchIO m => String -> (Connection -> m a) -> m a
withSqliteConn = withSqlConn . open'

open' :: String -> IO Connection
open' s = do
    conn <- Sqlite.open s
    smap <- newIORef $ Map.empty
    return Connection
        { prepare = prepare' conn
        , stmtMap = smap
        , insertSql = insertSql'
        , close = Sqlite.close conn
        , migrateSql = migrate'
        , begin = helper "BEGIN"
        , commit = helper "COMMIT"
        , rollback = helper "ROLLBACK"
        }
  where
    helper t getter = do
        stmt <- getter t
        execute stmt []

prepare' :: Sqlite.Connection -> String -> IO Statement
prepare' conn sql = do
    stmt <- Sqlite.prepare conn sql
    return Statement
        { finalize = Sqlite.finalize stmt
        , reset = Sqlite.reset stmt
        , execute = execute' stmt
        , withStmt = withStmt' stmt
        }

insertSql' :: String -> [String] -> Either String (String, String)
insertSql' t cols =
    Right (ins, sel)
  where
    sel = "SELECT last_insert_rowid()"
    ins = concat
        [ "INSERT INTO "
        , t
        , "("
        , intercalate "," cols
        , ") VALUES("
        , intercalate "," (map (const "?") cols)
        , ")"
        ]

execute' :: Sqlite.Statement -> [PersistValue] -> IO ()
execute' stmt vals = do
    Sqlite.bind stmt vals
    Sqlite.Done <- Sqlite.step stmt
    return ()

withStmt' :: MonadCatchIO m
          => Sqlite.Statement
          -> [PersistValue]
          -> (RowPopper m -> m a)
          -> m a
withStmt' stmt vals f = do
    liftIO $ Sqlite.bind stmt vals
    x <- f go
    liftIO $ Sqlite.reset stmt
    return x
  where
    go = liftIO $ do
        x <- Sqlite.step stmt
        case x of
            Sqlite.Done -> return Nothing
            Sqlite.Row -> do
                cols <- liftIO $ Sqlite.columns stmt
                return $ Just cols
showSqlType :: SqlType -> String
showSqlType SqlString = "VARCHAR"
showSqlType SqlInteger = "INTEGER"
showSqlType SqlReal = "REAL"
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlBlob = "BLOB"
showSqlType SqlBool = "BOOLEAN"

migrate' :: PersistEntity val
         => (String -> IO Statement)
         -> val
         -> IO (Either [String] [(Bool, String)])
migrate' getter val = do
    let (cols, uniqs) = mkColumns val
    let newSql = mkCreateTable False table (cols, uniqs)
    stmt <- getter $ "SELECT sql FROM sqlite_master WHERE " ++
                        "type='table' AND name=?"
    oldSql' <- withStmt stmt [PersistString table] go
    case oldSql' of
        Nothing -> return $ Right [(False, newSql)]
        Just oldSql ->
            if oldSql == newSql
                then return $ Right []
                else do
                    sql <- getCopyTable getter val
                    return $ Right $ map (\x -> (False, x)) sql
  where
    def = entityDef val
    table = tableName def
    go pop = do
        x <- pop
        case x of
            Nothing -> return Nothing
            Just [PersistString y] -> return $ Just y
            Just y -> error $ "Unexpected result from sqlite_master: " ++ show y

getCopyTable :: PersistEntity val => (String -> IO Statement) -> val -> IO [Sql]
getCopyTable getter val = do
    stmt <- getter $ "PRAGMA table_info(" ++ table ++ ")"
    oldCols' <- withStmt stmt [] getCols
    let oldCols = map (map toLower) $ filter (/= "id") oldCols'
    let newCols = map (map toLower . cName) cols :: [String]
    let common = filter (`elem` oldCols) newCols :: [String]
    if common /= oldCols
        then error $ "Migrating table " ++ table ++ " would drop columns."
        else return
                [ tmpSql
                , copyToTemp $ "id" : common
                , dropOld
                , newSql
                , copyToFinal $ "id" : newCols
                , dropTmp
                ]
  where
    def = entityDef val
    getCols pop = do
        x <- pop
        case x of
            Nothing -> return []
            Just (_:PersistString name:_) -> do
                names <- getCols pop
                return $ name : names
            Just y -> error $ "Invalid result from PRAGMA table_info: " ++ show y
    table = tableName def
    tableTmp = table ++ "_backup"
    (cols, uniqs) = mkColumns val
    newSql = mkCreateTable False table (cols, uniqs)
    tmpSql = mkCreateTable True tableTmp (cols, uniqs)
    dropTmp = "DROP TABLE " ++ tableTmp
    dropOld = "DROP TABLE " ++ table
    copyToTemp common = concat
        [ "INSERT INTO "
        , tableTmp
        , "("
        , intercalate "," common
        , ") SELECT "
        , intercalate "," common
        , " FROM "
        , table
        ]
    copyToFinal newCols = concat
        [ "INSERT INTO "
        , table
        , " SELECT "
        , intercalate "," newCols
        , " FROM "
        , tableTmp
        ]

mkCreateTable :: Bool -> String -> ([Column], [UniqueDef]) -> Sql
mkCreateTable isTemp table (cols, uniqs) = concat
    [ "CREATE"
    , if isTemp then " TEMP" else ""
    , " TABLE "
    , table
    , "(id INTEGER PRIMARY KEY"
    , concatMap sqlColumn cols
    , concatMap sqlUnique uniqs
    , ")"
    ]

sqlColumn :: Column -> String
sqlColumn (Column name isNull typ def ref) = concat
    [ ","
    , name
    , " "
    , showSqlType typ
    , if isNull then " NULL" else " NOT NULL"
    , case def of
        Nothing -> ""
        Just d -> " DEFAULT " ++ d
    , case ref of
        Nothing -> ""
        Just (table, _) -> " REFERENCES " ++ table
    ]

sqlUnique :: (String, [String]) -> String
sqlUnique (cname, cols) = concat
    [ ",CONSTRAINT "
    , cname
    , " UNIQUE ("
    , intercalate "," cols
    , ")"
    ]

type Sql = String
