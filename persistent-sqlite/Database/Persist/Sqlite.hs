{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A sqlite backend for persistent.
module Database.Persist.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , createSqlitePool
    , module Database.Persist
    , module Database.Persist.GenericSql
    , SqliteConf (..)
    , runSqlite
    ) where

import Database.Persist hiding (Entity (..))
import Database.Persist.Store
import Database.Persist.EntityDef
import Database.Persist.GenericSql hiding (Key)
import Database.Persist.GenericSql.Internal

import qualified Database.Sqlite as Sqlite

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Trans.Control (control)
import qualified Control.Exception as E
import Data.Text (Text, pack)
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Applicative
import Data.Int (Int64)

createSqlitePool :: MonadIO m => Text -> Int -> m ConnectionPool
createSqlitePool s = createSqlPool $ open' s

withSqlitePool :: (MonadBaseControl IO m, MonadIO m)
               => Text
               -> Int -- ^ number of connections to open
               -> (ConnectionPool -> m a) -> m a
withSqlitePool s = withSqlPool $ open' s

withSqliteConn :: (MonadBaseControl IO m, MonadIO m)
               => Text -> (Connection -> m a) -> m a
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
        , rollbackC = ignoreExceptions . helper "ROLLBACK"
        , escapeName = escape
        , noLimit = "LIMIT -1"
        }
  where
    helper t getter = do
        stmt <- getter t
        _ <- execute stmt []
        reset stmt
    ignoreExceptions = E.handle (\(_ :: E.SomeException) -> return ())

-- | A convenience helper which creates a new database connection and runs the
-- given block, handling @MonadResource@ and @MonadLogger@ requirements. Note
-- that all log messages are discarded.
--
-- Since 1.1.4
runSqlite :: (MonadBaseControl IO m, MonadIO m)
          => Text -- ^ connection string
          -> SqlPersist (NoLoggingT (ResourceT m)) a -- ^ database action
          -> m a
runSqlite connstr = runResourceT
                  . runNoLoggingT
                  . withSqliteConn connstr
                  . runSqlConn

prepare' :: Sqlite.Connection -> Text -> IO Statement
prepare' conn sql = do
    stmt <- Sqlite.prepare conn sql
    return Statement
        { finalize = Sqlite.finalize stmt
        , reset = Sqlite.reset conn stmt
        , execute = execute' conn stmt
        , withStmt = withStmt' conn stmt
        }

insertSql' :: DBName -> [DBName] -> DBName -> InsertSqlResult
insertSql' t cols _ =
    ISRInsertGet (pack ins) sel
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

execute' :: Sqlite.Connection -> Sqlite.Statement -> [PersistValue] -> IO Int64
execute' conn stmt vals = flip finally (liftIO $ Sqlite.reset conn stmt) $ do
    Sqlite.bind stmt vals
    Sqlite.Done <- Sqlite.step stmt
    Sqlite.changes conn

withStmt'
          :: MonadResource m
          => Sqlite.Connection
          -> Sqlite.Statement
          -> [PersistValue]
          -> Source m [PersistValue]
withStmt' conn stmt vals = bracketP
    (Sqlite.bind stmt vals >> return stmt)
    (Sqlite.reset conn)
    (const pull)
  where
    pull = do
        x <- liftIO $ Sqlite.step stmt
        case x of
            Sqlite.Done -> return ()
            Sqlite.Row -> do
                cols <- liftIO $ Sqlite.columns stmt
                yield cols
                pull
showSqlType :: SqlType -> String
showSqlType SqlString = "VARCHAR"
showSqlType SqlInt32 = "INTEGER"
showSqlType SqlInt64 = "INTEGER"
showSqlType SqlReal = "REAL"
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTimeZoned = "TIMESTAMP"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlBlob = "BLOB"
showSqlType SqlBool = "BOOLEAN"
showSqlType (SqlOther t) = T.unpack t

migrate' :: PersistEntity val
         => [EntityDef]
         -> (Text -> IO Statement)
         -> val
         -> IO (Either [Text] [(Bool, Text)])
migrate' allDefs getter val = do
    let (cols, uniqs) = mkColumns allDefs val
    let newSql = mkCreateTable False def (cols, uniqs)
    stmt <- getter "SELECT sql FROM sqlite_master WHERE type='table' AND name=?"
    oldSql' <- runResourceT
             $ withStmt stmt [PersistText $ unDBName table] $$ go
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
    oldCols' <- runResourceT $ withStmt stmt [] $$ getCols
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
sqlColumn (Column name isNull typ def _maxLen ref) = concat
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
sqlUnique (UniqueDef _ cname cols _) = concat
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

finally :: MonadBaseControl IO m
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was raised)
        -> m a
finally a sequel = control $ \runInIO ->
                     E.finally (runInIO a)
                               (runInIO sequel)
{-# INLINABLE finally #-}
