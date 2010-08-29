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
        , escapeName = escape
        , noLimit = "LIMIT -1"
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

insertSql' :: RawName -> [RawName] -> Either String (String, String)
insertSql' t cols =
    Right (ins, sel)
  where
    sel = "SELECT last_insert_rowid()"
    ins = concat
        [ "INSERT INTO "
        , escape t
        , "("
        , intercalate "," $ map escape cols
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
    oldSql' <- withStmt stmt [PersistString $ unRawName table] go
    case oldSql' of
        Nothing -> return $ Right [(False, newSql)]
        Just oldSql ->
            if oldSql == newSql
                then return $ Right []
                else do
                    sql <- getCopyTable getter val
                    return $ Right sql
  where
    def = entityDef val
    table = rawTableName def
    go pop = do
        x <- pop
        case x of
            Nothing -> return Nothing
            Just [PersistString y] -> return $ Just y
            Just y -> error $ "Unexpected result from sqlite_master: " ++ show y

getCopyTable :: PersistEntity val => (String -> IO Statement) -> val
             -> IO [(Bool, Sql)]
getCopyTable getter val = do
    stmt <- getter $ "PRAGMA table_info(" ++ escape table ++ ")"
    oldCols' <- withStmt stmt [] getCols
    let oldCols = map RawName $ filter (/= "id") oldCols'
    let newCols = map cName cols
    let common = filter (`elem` oldCols) newCols
    return [ (False, tmpSql)
           , (False, copyToTemp $ RawName "id" : common)
           , (common /= oldCols, dropOld)
           , (False, newSql)
           , (False, copyToFinal $ RawName "id" : newCols)
           , (False, dropTmp)
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
    table = rawTableName def
    tableTmp = RawName $ unRawName table ++ "_backup"
    (cols, uniqs) = mkColumns val
    newSql = mkCreateTable False table (cols, uniqs)
    tmpSql = mkCreateTable True tableTmp (cols, uniqs)
    dropTmp = "DROP TABLE " ++ escape tableTmp
    dropOld = "DROP TABLE " ++ escape table
    copyToTemp common = concat
        [ "INSERT INTO "
        , escape tableTmp
        , "("
        , intercalate "," $ map escape common
        , ") SELECT "
        , intercalate "," $ map escape common
        , " FROM "
        , escape table
        ]
    copyToFinal newCols = concat
        [ "INSERT INTO "
        , escape table
        , " SELECT "
        , intercalate "," $ map escape newCols
        , " FROM "
        , escape tableTmp
        ]

mkCreateTable :: Bool -> RawName -> ([Column], [UniqueDef]) -> Sql
mkCreateTable isTemp table (cols, uniqs) = concat
    [ "CREATE"
    , if isTemp then " TEMP" else ""
    , " TABLE "
    , escape table
    , "(id INTEGER PRIMARY KEY"
    , concatMap sqlColumn cols
    , concatMap sqlUnique uniqs
    , ")"
    ]

sqlColumn :: Column -> String
sqlColumn (Column name isNull typ def ref) = concat
    [ ","
    , escape name
    , " "
    , showSqlType typ
    , if isNull then " NULL" else " NOT NULL"
    , case def of
        Nothing -> ""
        Just d -> " DEFAULT " ++ d
    , case ref of
        Nothing -> ""
        Just (table, _) -> " REFERENCES " ++ escape table
    ]

sqlUnique :: UniqueDef -> String
sqlUnique (cname, cols) = concat
    [ ",CONSTRAINT "
    , escape cname
    , " UNIQUE ("
    , intercalate "," $ map escape cols
    , ")"
    ]

type Sql = String

escape :: RawName -> String
escape (RawName s) =
    '"' : go s ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs
