{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | A sqlite backend for persistent.
module Database.Persist.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , createSqlitePool
    , module Database.Persist
    , module Database.Persist.GenericSql
    , SqliteConf (..)
    ) where

import Database.Persist hiding (Entity (..))
import Database.Persist.Store
import Database.Persist.EntityDef
import Database.Persist.GenericSql hiding (Key(..))
import Database.Persist.GenericSql.Internal
import Database.Persist.Query.GenericSql ()

import qualified Database.Sqlite as Sqlite

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception as E
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (finally)
#define MBCIO MonadControlIO
#endif
import Data.Text (Text, pack)
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Applicative

createSqlitePool :: MonadIO m => Text -> Int -> m ConnectionPool
createSqlitePool s = createSqlPool $ open' s

withSqlitePool :: C.ResourceIO m
               => Text
               -> Int -- ^ number of connections to open
               -> (ConnectionPool -> m a) -> m a
withSqlitePool s = withSqlPool $ open' s

withSqliteConn :: C.ResourceIO m => Text -> (Connection -> m a) -> m a
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

insertSql' :: DBName -> [DBName] -> Either Text (Text, Text)
insertSql' t cols =
    Right (pack ins, sel)
  where
    sel = "SELECT last_insert_rowid()"
    ins = concat
        [ "INSERT INTO "
        , escape' t
        , "("
        , intercalate "," $ map escape' cols
        , ") VALUES("
        , intercalate "," (map (const "?") cols)
        , ")"
        ]

execute' :: Sqlite.Statement -> [PersistValue] -> IO ()
execute' stmt vals = flip finally (liftIO $ Sqlite.reset stmt) $ do
    Sqlite.bind stmt vals
    Sqlite.Done <- Sqlite.step stmt
    return ()

withStmt'
          :: C.ResourceIO m
          => Sqlite.Statement
          -> [PersistValue]
          -> C.Source m [PersistValue]
withStmt' stmt vals = C.sourceIO
    (Sqlite.bind stmt vals >> return stmt)
    Sqlite.reset
    pull
  where
    pull _ = liftIO $ do
        x <- Sqlite.step stmt
        case x of
            Sqlite.Done -> return C.Closed
            Sqlite.Row -> do
                cols <- liftIO $ Sqlite.columns stmt
                return $ C.Open cols
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
         => [EntityDef]
         -> (Text -> IO Statement)
         -> val
         -> IO (Either [Text] [(Bool, Text)])
migrate' allDefs getter val = do
    let (cols, uniqs) = mkColumns allDefs val
    let newSql = mkCreateTable False def (cols, uniqs)
    stmt <- getter "SELECT sql FROM sqlite_master WHERE type='table' AND name=?"
    oldSql' <- C.runResourceT
             $ withStmt stmt [PersistText $ unDBName table] C.$$ go
    case oldSql' of
        Nothing -> return $ Right [(False, newSql)]
        Just oldSql ->
            if oldSql == newSql
                then return $ Right []
                else do
                    sql <- getCopyTable allDefs getter val
                    return $ Right sql
  where
    def = entityDef val
    table = entityDB def
    go = do
        x <- CL.head
        case x of
            Nothing -> return Nothing
            Just [PersistText y] -> return $ Just y
            Just y -> error $ "Unexpected result from sqlite_master: " ++ show y

getCopyTable :: PersistEntity val
             => [EntityDef]
             -> (Text -> IO Statement)
             -> val
             -> IO [(Bool, Sql)]
getCopyTable allDefs getter val = do
    stmt <- getter $ pack $ "PRAGMA table_info(" ++ escape' table ++ ")"
    oldCols' <- C.runResourceT $ withStmt stmt [] C.$$ getCols
    let oldCols = map DBName $ filter (/= "id") oldCols' -- need to update for table id attribute ?
    let newCols = map cName cols
    let common = filter (`elem` oldCols) newCols
    let id_ = entityID $ entityDef val
    return [ (False, tmpSql)
           , (False, copyToTemp $ id_ : common)
           , (common /= oldCols, pack dropOld)
           , (False, newSql)
           , (False, copyToFinal $ id_ : newCols)
           , (False, pack dropTmp)
           ]
  where
    def = entityDef val
    getCols = do
        x <- CL.head
        case x of
            Nothing -> return []
            Just (_:PersistText name:_) -> do
                names <- getCols
                return $ name : names
            Just y -> error $ "Invalid result from PRAGMA table_info: " ++ show y
    table = entityDB def
    tableTmp = DBName $ unDBName table `T.append` "_backup"
    (cols, uniqs) = mkColumns allDefs val
    newSql = mkCreateTable False def (cols, uniqs)
    tmpSql = mkCreateTable True def { entityDB = tableTmp } (cols, uniqs)
    dropTmp = "DROP TABLE " ++ escape' tableTmp
    dropOld = "DROP TABLE " ++ escape' table
    copyToTemp common = pack $ concat
        [ "INSERT INTO "
        , escape' tableTmp
        , "("
        , intercalate "," $ map escape' common
        , ") SELECT "
        , intercalate "," $ map escape' common
        , " FROM "
        , escape' table
        ]
    copyToFinal newCols = pack $ concat
        [ "INSERT INTO "
        , T.unpack $ escape table
        , " SELECT "
        , intercalate "," $ map escape' newCols
        , " FROM "
        , escape' tableTmp
        ]

escape' :: DBName -> String
escape' = T.unpack . escape

mkCreateTable :: Bool -> EntityDef -> ([Column], [UniqueDef]) -> Sql
mkCreateTable isTemp entity (cols, uniqs) = pack $ concat
    [ "CREATE"
    , if isTemp then " TEMP" else ""
    , " TABLE "
    , T.unpack $ escape $ entityDB entity
    , "("
    , T.unpack $ escape $ entityID entity
    , " INTEGER PRIMARY KEY"
    , concatMap sqlColumn cols
    , concatMap sqlUnique uniqs
    , ")"
    ]

sqlColumn :: Column -> String
sqlColumn (Column name isNull typ def ref) = concat
    [ ","
    , T.unpack $ escape name
    , " "
    , showSqlType typ
    , if isNull then " NULL" else " NOT NULL"
    , case def of
        Nothing -> ""
        Just d -> " DEFAULT " ++ T.unpack d
    , case ref of
        Nothing -> ""
        Just (table, _) -> " REFERENCES " ++ T.unpack (escape table)
    ]

sqlUnique :: UniqueDef -> String
sqlUnique (UniqueDef _ cname cols) = concat
    [ ",CONSTRAINT "
    , T.unpack $ escape cname
    , " UNIQUE ("
    , intercalate "," $ map (T.unpack . escape . snd) cols
    , ")"
    ]

type Sql = Text

escape :: DBName -> Text
escape (DBName s) =
    T.concat [q, T.concatMap go s, q]
  where
    q = T.singleton '"'
    go '"' = "\"\""
    go c = T.singleton c

-- | Information required to connect to a sqlite database
data SqliteConf = SqliteConf
    { sqlDatabase :: Text
    , sqlPoolSize :: Int
    }

instance PersistConfig SqliteConf where
    type PersistConfigBackend SqliteConf = SqlPersist
    type PersistConfigPool SqliteConf = ConnectionPool
    createPoolConfig (SqliteConf cs size) = createSqlitePool cs size
    runPool _ = runSqlPool
    loadConfig (Object o) =
        SqliteConf <$> o .: "database"
                   <*> o .: "poolsize"
    loadConfig _ = mzero

#if MIN_VERSION_monad_control(0, 3, 0)
finally :: MonadBaseControl IO m
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was raised)
        -> m a
finally a sequel = control $ \runInIO ->
                     E.finally (runInIO a)
                               (runInIO sequel)
{-# INLINABLE finally #-}
#endif
