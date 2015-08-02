{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
-- | A sqlite backend for persistent.
--
-- Note: If you prepend @WAL=off @ to your connection string, it will disable
-- the write-ahead log. For more information, see
-- <https://github.com/yesodweb/persistent/issues/363>.
module Database.Persist.Sqlite
    ( withSqlitePool
    , withSqliteConn
    , createSqlitePool
    , module Database.Persist.Sql
    , SqliteConf (..)
    , runSqlite
    , wrapConnection
    ) where

import Database.Persist.Sql

import qualified Database.Sqlite as Sqlite

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (NoLoggingT, runNoLoggingT, MonadLogger)
import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Trans.Control (control)
import Data.Acquire (Acquire, mkAcquire, with)
import qualified Control.Exception as E
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Applicative
import Data.Int (Int64)
import Data.Monoid ((<>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad (when)

createSqlitePool :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => Text -> Int -> m ConnectionPool
createSqlitePool s = createSqlPool $ open' s

withSqlitePool :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
               => Text
               -> Int -- ^ number of connections to open
               -> (ConnectionPool -> m a) -> m a
withSqlitePool s = withSqlPool $ open' s

withSqliteConn :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
               => Text -> (SqlBackend -> m a) -> m a
withSqliteConn = withSqlConn . open'

open' :: Text -> LogFunc -> IO SqlBackend
open' connStr logFunc = do
    let (connStr', enableWal) = case () of
          ()
            | Just cs <- T.stripPrefix "WAL=on "  connStr -> (cs, True)
            | Just cs <- T.stripPrefix "WAL=off " connStr -> (cs, False)
            | otherwise                                   -> (connStr, True)

    conn <- Sqlite.open connStr'
    wrapConnectionWal enableWal conn logFunc

-- | Wrap up a raw 'Sqlite.Connection' as a Persistent SQL 'Connection'.
--
-- Since 1.1.5
wrapConnection :: Sqlite.Connection -> LogFunc -> IO SqlBackend
wrapConnection = wrapConnectionWal True

-- | Allow control of WAL settings when wrapping
wrapConnectionWal :: Bool -- ^ enable WAL?
                  -> Sqlite.Connection
                  -> LogFunc
                  -> IO SqlBackend
wrapConnectionWal enableWal conn logFunc = do
    when enableWal $ do
        -- Turn on the write-ahead log
        -- https://github.com/yesodweb/persistent/issues/363
        turnOnWal <- Sqlite.prepare conn "PRAGMA journal_mode=WAL;"
        _ <- Sqlite.step turnOnWal
        Sqlite.reset conn turnOnWal
        Sqlite.finalize turnOnWal

    smap <- newIORef $ Map.empty
    return SqlBackend
        { connPrepare = prepare' conn
        , connStmtMap = smap
        , connInsertSql = insertSql'
        , connInsertManySql = Nothing
        , connClose = Sqlite.close conn
        , connMigrateSql = migrate'
        , connBegin = helper "BEGIN"
        , connCommit = helper "COMMIT"
        , connRollback = ignoreExceptions . helper "ROLLBACK"
        , connEscapeName = escape
        , connNoLimit = "LIMIT -1"
        , connRDBMS = "sqlite"
        , connLimitOffset = decorateSQLWithLimitOffset "LIMIT -1"
        , connLogFunc = logFunc
        }
  where
    helper t getter = do
        stmt <- getter t
        _ <- stmtExecute stmt []
        stmtReset stmt
    ignoreExceptions = E.handle (\(_ :: E.SomeException) -> return ())

-- | A convenience helper which creates a new database connection and runs the
-- given block, handling @MonadResource@ and @MonadLogger@ requirements. Note
-- that all log messages are discarded.
--
-- Since 1.1.4
runSqlite :: (MonadBaseControl IO m, MonadIO m)
          => Text -- ^ connection string
          -> SqlPersistT (NoLoggingT (ResourceT m)) a -- ^ database action
          -> m a
runSqlite connstr = runResourceT
                  . runNoLoggingT
                  . withSqliteConn connstr
                  . runSqlConn

prepare' :: Sqlite.Connection -> Text -> IO Statement
prepare' conn sql = do
    stmt <- Sqlite.prepare conn sql
    return Statement
        { stmtFinalize = Sqlite.finalize stmt
        , stmtReset = Sqlite.reset conn stmt
        , stmtExecute = execute' conn stmt
        , stmtQuery = withStmt' conn stmt
        }

insertSql' :: EntityDef -> [PersistValue] -> InsertSqlResult
insertSql' ent vals =
  case entityPrimary ent of
    Just _ ->
      ISRManyKeys sql vals
        where sql = T.concat
                [ "INSERT INTO "
                , escape $ entityDB ent
                , "("
                , T.intercalate "," $ map (escape . fieldDB) $ entityFields ent
                , ") VALUES("
                , T.intercalate "," (map (const "?") $ entityFields ent)
                , ")"
                ]
    Nothing ->
      ISRInsertGet ins sel
        where
          sel = T.concat
              [ "SELECT "
              , escape $ fieldDB (entityId ent)
              , " FROM "
              , escape $ entityDB ent
              , " WHERE _ROWID_=last_insert_rowid()"
              ]
          ins = T.concat
              [ "INSERT INTO "
              , escape $ entityDB ent
              , if null (entityFields ent)
                    then " VALUES(null)"
                    else T.concat
                      [ "("
                      , T.intercalate "," $ map (escape . fieldDB) $ entityFields ent
                      , ") VALUES("
                      , T.intercalate "," (map (const "?") $ entityFields ent)
                      , ")"
                      ]
              ]

execute' :: Sqlite.Connection -> Sqlite.Statement -> [PersistValue] -> IO Int64
execute' conn stmt vals = flip finally (liftIO $ Sqlite.reset conn stmt) $ do
    Sqlite.bind stmt vals
    _ <- Sqlite.step stmt
    Sqlite.changes conn

withStmt'
          :: MonadIO m
          => Sqlite.Connection
          -> Sqlite.Statement
          -> [PersistValue]
          -> Acquire (Source m [PersistValue])
withStmt' conn stmt vals = do
    _ <- mkAcquire
        (Sqlite.bind stmt vals >> return stmt)
        (Sqlite.reset conn)
    return pull
  where
    pull = do
        x <- liftIO $ Sqlite.step stmt
        case x of
            Sqlite.Done -> return ()
            Sqlite.Row -> do
                cols <- liftIO $ Sqlite.columns stmt
                yield cols
                pull

showSqlType :: SqlType -> Text
showSqlType SqlString = "VARCHAR"
showSqlType SqlInt32 = "INTEGER"
showSqlType SqlInt64 = "INTEGER"
showSqlType SqlReal = "REAL"
showSqlType (SqlNumeric precision scale) = T.concat [ "NUMERIC(", T.pack (show precision), ",", T.pack (show scale), ")" ]
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlBlob = "BLOB"
showSqlType SqlBool = "BOOLEAN"
showSqlType SqlArray = "VARCHAR" -- serialized to JSON
showSqlType SqlMap = "VARCHAR" -- serialized to JSON
showSqlType (SqlOther t) = t

migrate' :: [EntityDef]
         -> (Text -> IO Statement)
         -> EntityDef
         -> IO (Either [Text] [(Bool, Text)])
migrate' allDefs getter val = do
    let (cols, uniqs, _) = mkColumns allDefs val
    let newSql = mkCreateTable False def (filter (not . safeToRemove val . cName) cols, uniqs)
    stmt <- getter "SELECT sql FROM sqlite_master WHERE type='table' AND name=?"
    oldSql' <- with (stmtQuery stmt [PersistText $ unDBName table]) ($$ go)
    case oldSql' of
        Nothing -> return $ Right [(False, newSql)]
        Just oldSql -> do
            if oldSql == newSql
                then return $ Right []
                else do
                    sql <- getCopyTable allDefs getter val
                    return $ Right sql
  where
    def = val
    table = entityDB def
    go = do
        x <- CL.head
        case x of
            Nothing -> return Nothing
            Just [PersistText y] -> return $ Just y
            Just y -> error $ "Unexpected result from sqlite_master: " ++ show y

-- | Check if a column name is listed as the "safe to remove" in the entity
-- list.
safeToRemove :: EntityDef -> DBName -> Bool
safeToRemove def (DBName colName)
    = any (elem "SafeToRemove" . fieldAttrs)
    $ filter ((== DBName colName) . fieldDB)
    $ entityFields def

getCopyTable :: [EntityDef]
             -> (Text -> IO Statement)
             -> EntityDef
             -> IO [(Bool, Text)]
getCopyTable allDefs getter def = do
    stmt <- getter $ T.concat [ "PRAGMA table_info(", escape table, ")" ]
    oldCols' <- with (stmtQuery stmt []) ($$ getCols)
    let oldCols = map DBName $ filter (/= "id") oldCols' -- need to update for table id attribute ?
    let newCols = filter (not . safeToRemove def) $ map cName cols
    let common = filter (`elem` oldCols) newCols
    let id_ = fieldDB (entityId def)
    return [ (False, tmpSql)
           , (False, copyToTemp $ id_ : common)
           , (common /= filter (not . safeToRemove def) oldCols, dropOld)
           , (False, newSql)
           , (False, copyToFinal $ id_ : newCols)
           , (False, dropTmp)
           ]
  where
    getCols = do
        x <- CL.head
        case x of
            Nothing -> return []
            Just (_:PersistText name:_) -> do
                names <- getCols
                return $ name : names
            Just y -> error $ "Invalid result from PRAGMA table_info: " ++ show y
    table = entityDB def
    tableTmp = DBName $ unDBName table <> "_backup"
    (cols, uniqs, _) = mkColumns allDefs def
    cols' = filter (not . safeToRemove def . cName) cols
    newSql = mkCreateTable False def (cols', uniqs)
    tmpSql = mkCreateTable True def { entityDB = tableTmp } (cols', uniqs)
    dropTmp = "DROP TABLE " <> escape tableTmp
    dropOld = "DROP TABLE " <> escape table
    copyToTemp common = T.concat
        [ "INSERT INTO "
        , escape tableTmp
        , "("
        , T.intercalate "," $ map escape common
        , ") SELECT "
        , T.intercalate "," $ map escape common
        , " FROM "
        , escape table
        ]
    copyToFinal newCols = T.concat
        [ "INSERT INTO "
        , escape table
        , " SELECT "
        , T.intercalate "," $ map escape newCols
        , " FROM "
        , escape tableTmp
        ]

mkCreateTable :: Bool -> EntityDef -> ([Column], [UniqueDef]) -> Text
mkCreateTable isTemp entity (cols, uniqs) =
  case entityPrimary entity of
    Just pdef ->
       T.concat
        [ "CREATE"
        , if isTemp then " TEMP" else ""
        , " TABLE "
        , escape $ entityDB entity
        , "("
        , T.drop 1 $ T.concat $ map sqlColumn cols
        , ", PRIMARY KEY "
        , "("
        , T.intercalate "," $ map (escape . fieldDB) $ compositeFields pdef
        , ")"
        , ")"
        ]
    Nothing -> T.concat
        [ "CREATE"
        , if isTemp then " TEMP" else ""
        , " TABLE "
        , escape $ entityDB entity
        , "("
        , escape $ fieldDB (entityId entity)
        , " "
        , showSqlType $ fieldSqlType $ entityId entity
        ," PRIMARY KEY"
        , mayDefault $ defaultAttribute $ fieldAttrs $ entityId entity
        , T.concat $ map sqlColumn cols
        , T.concat $ map sqlUnique uniqs
        , ")"
        ]

mayDefault :: Maybe Text -> Text
mayDefault def = case def of
    Nothing -> ""
    Just d -> " DEFAULT " <> d

sqlColumn :: Column -> Text
sqlColumn (Column name isNull typ def _cn _maxLen ref) = T.concat
    [ ","
    , escape name
    , " "
    , showSqlType typ
    , if isNull then " NULL" else " NOT NULL"
    , mayDefault def
    , case ref of
        Nothing -> ""
        Just (table, _) -> " REFERENCES " <> escape table
    ]

sqlUnique :: UniqueDef -> Text
sqlUnique (UniqueDef _ cname cols _) = T.concat
    [ ",CONSTRAINT "
    , escape cname
    , " UNIQUE ("
    , T.intercalate "," $ map (escape . snd) cols
    , ")"
    ]

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
    } deriving Show

instance FromJSON SqliteConf where
    parseJSON v = modifyFailure ("Persistent: error loading Sqlite conf: " ++) $
      flip (withObject "SqliteConf") v $ \o -> SqliteConf
        <$> o .: "database"
        <*> o .: "poolsize"
instance PersistConfig SqliteConf where
    type PersistConfigBackend SqliteConf = SqlPersistT
    type PersistConfigPool SqliteConf = ConnectionPool
    createPoolConfig (SqliteConf cs size) = runNoLoggingT $ createSqlitePool cs size -- FIXME
    runPool _ = runSqlPool
    loadConfig = parseJSON

finally :: MonadBaseControl IO m
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was raised)
        -> m a
finally a sequel = control $ \runInIO ->
                     E.finally (runInIO a)
                               (runInIO sequel)
{-# INLINABLE finally #-}
