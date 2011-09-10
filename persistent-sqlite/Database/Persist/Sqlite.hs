{-# LANGUAGE OverloadedStrings #-}
-- | A sqlite backend for persistent.
module Database.Persist.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , module Database.Persist
    , module Database.Persist.GenericSql
    ) where

import Database.Persist
import Database.Persist.Base
import Database.Persist.GenericSql hiding (Key(..))
import Database.Persist.GenericSql.Internal

import qualified Database.Sqlite as Sqlite

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (finally)
import Data.Text (Text, pack, unpack)

withSqlitePool :: MonadControlIO m
               => Text
               -> Int -- ^ number of connections to open
               -> (ConnectionPool -> m a) -> m a
withSqlitePool s = withSqlPool $ open' s

withSqliteConn :: MonadControlIO m => Text -> (Connection -> m a) -> m a
withSqliteConn = withSqlConn . open'

open' :: Text -> IO Connection
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
        , commitC = helper "COMMIT"
        , rollbackC = helper "ROLLBACK"
        , escapeName = escape
        , noLimit = "LIMIT -1"
        }
  where
    helper t getter = do
        stmt <- getter t
        execute stmt []
        reset stmt

prepare' :: Sqlite.Connection -> Text -> IO Statement
prepare' conn sql = do
    stmt <- Sqlite.prepare conn sql
    return Statement
        { finalize = Sqlite.finalize stmt
        , reset = Sqlite.reset stmt
        , execute = execute' stmt
        , withStmt = withStmt' stmt
        }

insertSql' :: RawName -> [RawName] -> Either Text (Text, Text)
insertSql' t cols =
    Right (pack ins, sel)
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
execute' stmt vals = flip finally (liftIO $ Sqlite.reset stmt) $ do
    Sqlite.bind stmt vals
    Sqlite.Done <- Sqlite.step stmt
    return ()

withStmt' :: MonadControlIO m
          => Sqlite.Statement
          -> [PersistValue]
          -> (RowPopper m -> m a)
          -> m a
withStmt' stmt vals f = flip finally (liftIO $ Sqlite.reset stmt) $ do
    liftIO $ Sqlite.bind stmt vals
    x <- f go
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
showSqlType SqlInt32 = "INTEGER"
showSqlType SqlInteger = "INTEGER"
showSqlType SqlReal = "REAL"
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlBlob = "BLOB"
showSqlType SqlBool = "BOOLEAN"

migrate' :: PersistEntity val
         => (Text -> IO Statement)
         -> val
         -> IO (Either [Text] [(Bool, Text)])
migrate' getter val = do
    let (cols, uniqs) = mkColumns val
    let newSql = mkCreateTable False table (cols, uniqs)
    stmt <- getter "SELECT sql FROM sqlite_master WHERE type='table' AND name=?"
    oldSql' <- withStmt stmt [PersistText $ pack $ unRawName table] go
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
            Just [PersistText y] -> return $ Just y
            Just y -> error $ "Unexpected result from sqlite_master: " ++ show y

getCopyTable :: PersistEntity val => (Text -> IO Statement) -> val
             -> IO [(Bool, Sql)]
getCopyTable getter val = do
    stmt <- getter $ pack $ "PRAGMA table_info(" ++ escape table ++ ")"
    oldCols' <- withStmt stmt [] getCols
    let oldCols = map (RawName . unpack) $ filter (/= "id") oldCols' -- need to update for table id attribute ?
    let newCols = map cName cols
    let common = filter (`elem` oldCols) newCols
    let id_ = rawTableIdName $ entityDef val
    return [ (False, tmpSql)
           , (False, copyToTemp $ id_ : common)
           , (common /= oldCols, pack dropOld)
           , (False, newSql)
           , (False, copyToFinal $ id_ : newCols)
           , (False, pack dropTmp)
           ]
  where
    def = entityDef val
    getCols pop = do
        x <- pop
        case x of
            Nothing -> return []
            Just (_:PersistText name:_) -> do
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
    copyToTemp common = pack $ concat
        [ "INSERT INTO "
        , escape tableTmp
        , "("
        , intercalate "," $ map escape common
        , ") SELECT "
        , intercalate "," $ map escape common
        , " FROM "
        , escape table
        ]
    copyToFinal newCols = pack $ concat
        [ "INSERT INTO "
        , escape table
        , " SELECT "
        , intercalate "," $ map escape newCols
        , " FROM "
        , escape tableTmp
        ]

mkCreateTable :: Bool -> RawName -> ([Column], [UniqueDef']) -> Sql
mkCreateTable isTemp table (cols, uniqs) = pack $ concat
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

sqlUnique :: UniqueDef' -> String
sqlUnique (cname, cols) = concat
    [ ",CONSTRAINT "
    , escape cname
    , " UNIQUE ("
    , intercalate "," $ map escape cols
    , ")"
    ]

type Sql = Text

escape :: RawName -> String
escape (RawName s) =
    '"' : go s ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs
