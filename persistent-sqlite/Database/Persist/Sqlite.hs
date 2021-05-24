{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- Strictly, this could go as low as GHC 8.6.1, which is when DerivingVia was
-- introduced - this base version requires 8.6.5+
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

-- | A sqlite backend for persistent.
--
-- Note: If you prepend @WAL=off @ to your connection string, it will disable
-- the write-ahead log. This functionality is now deprecated in favour of using SqliteConnectionInfo.
module Database.Persist.Sqlite
    ( withSqlitePool
    , withSqlitePoolInfo
    , withSqliteConn
    , withSqliteConnInfo
    , createSqlitePool
    , createSqlitePoolFromInfo
    , module Database.Persist.Sql
    , SqliteConf (..)
    , SqliteConnectionInfo
    , mkSqliteConnectionInfo
    , sqlConnectionStr
    , walEnabled
    , fkEnabled
    , extraPragmas
    , runSqlite
    , runSqliteInfo
    , wrapConnection
    , wrapConnectionInfo
    , mockMigration
    , retryOnBusy
    , waitForDatabase
    , ForeignKeyViolation(..)
    , checkForeignKeys
    , RawSqlite
    , persistentBackend
    , rawSqliteConnection
    , withRawSqliteConnInfo
    , createRawSqlitePoolFromInfo
    , createRawSqlitePoolFromInfo_
    , withRawSqlitePoolInfo
    , withRawSqlitePoolInfo_
    ) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Monad (forM_)
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO, askRunInIO, withRunInIO, withUnliftIO, unliftIO, withRunInIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT, MonadLoggerIO, logWarn, runLoggingT, askLoggerIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
#if !MIN_VERSION_base(4,12,0)
import Control.Monad.Trans.Reader (withReaderT)
#endif
import Control.Monad.Trans.Writer (runWriterT)
import Data.Acquire (Acquire, mkAcquire, with)
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Lazy as HashMap
import Data.Int (Int64)
import Data.IORef
import qualified Data.Map as Map
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lens.Micro.TH (makeLenses)
import UnliftIO.Resource (ResourceT, runResourceT)
import Data.Foldable (toList)

#if MIN_VERSION_base(4,12,0)
import Database.Persist.Compatible
#endif
import Database.Persist.Sql
import Database.Persist.SqlBackend
import qualified Database.Persist.Sql.Util as Util
import qualified Database.Sqlite as Sqlite


-- | Create a pool of SQLite connections.
--
-- Note that this should not be used with the @:memory:@ connection string, as
-- the pool will regularly remove connections, destroying your database.
-- Instead, use 'withSqliteConn'.
createSqlitePool :: (MonadLoggerIO m, MonadUnliftIO m)
                 => Text -> Int -> m (Pool SqlBackend)
createSqlitePool = createSqlitePoolFromInfo . conStringToInfo

-- | Create a pool of SQLite connections.
--
-- Note that this should not be used with the @:memory:@ connection string, as
-- the pool will regularly remove connections, destroying your database.
-- Instead, use 'withSqliteConn'.
--
-- @since 2.6.2
createSqlitePoolFromInfo :: (MonadLoggerIO m, MonadUnliftIO m)
                         => SqliteConnectionInfo -> Int -> m (Pool SqlBackend)
createSqlitePoolFromInfo connInfo = createSqlPool $ openWith const connInfo

-- | Run the given action with a connection pool.
--
-- Like 'createSqlitePool', this should not be used with @:memory:@.
withSqlitePool :: (MonadUnliftIO m, MonadLoggerIO m)
               => Text
               -> Int -- ^ number of connections to open
               -> (Pool SqlBackend -> m a) -> m a
withSqlitePool connInfo = withSqlPool . openWith const $ conStringToInfo connInfo

-- | Run the given action with a connection pool.
--
-- Like 'createSqlitePool', this should not be used with @:memory:@.
--
-- @since 2.6.2
withSqlitePoolInfo :: (MonadUnliftIO m, MonadLoggerIO m)
                   => SqliteConnectionInfo
                   -> Int -- ^ number of connections to open
                   -> (Pool SqlBackend -> m a) -> m a
withSqlitePoolInfo connInfo = withSqlPool $ openWith const connInfo

withSqliteConn :: (MonadUnliftIO m, MonadLoggerIO m)
               => Text -> (SqlBackend -> m a) -> m a
withSqliteConn = withSqliteConnInfo . conStringToInfo

-- | @since 2.6.2
withSqliteConnInfo :: (MonadUnliftIO m, MonadLoggerIO m)
                   => SqliteConnectionInfo -> (SqlBackend -> m a) -> m a
withSqliteConnInfo = withSqlConn . openWith const

openWith :: (SqlBackend -> Sqlite.Connection -> r)
         -> SqliteConnectionInfo
         -> LogFunc
         -> IO r
openWith f connInfo logFunc = do
    conn <- Sqlite.open $ _sqlConnectionStr connInfo
    backend <- wrapConnectionInfo connInfo conn logFunc `E.onException` Sqlite.close conn
    return $ f backend conn

-- | Wrap up a raw 'Sqlite.Connection' as a Persistent SQL 'Connection'.
--
-- === __Example usage__
--
-- > {-# LANGUAGE GADTs #-}
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE QuasiQuotes #-}
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- >
-- > import Control.Monad.IO.Class  (liftIO)
-- > import Database.Persist
-- > import Database.Sqlite
-- > import Database.Persist.Sqlite
-- > import Database.Persist.TH
-- >
-- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- > Person
-- >   name String
-- >   age Int Maybe
-- >   deriving Show
-- > |]
-- >
-- > main :: IO ()
-- > main = do
-- >   conn <- open "/home/sibi/test.db"
-- >   (backend :: SqlBackend) <- wrapConnection conn (\_ _ _ _ -> return ())
-- >   flip runSqlPersistM backend $ do
-- >          runMigration migrateAll
-- >          insert_ $ Person "John doe" $ Just 35
-- >          insert_ $ Person "Hema" $ Just 36
-- >          (pers :: [Entity Person]) <- selectList [] []
-- >          liftIO $ print pers
-- >   close' backend
--
-- On executing it, you get this output:
--
-- > Migrating: CREATE TABLE "person"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"age" INTEGER NULL)
-- > [Entity {entityKey = PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = Person {personName = "John doe", personAge = Just 35}},Entity {entityKey = PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 2}}, entityVal = Person {personName = "Hema", personAge = Just 36}}]
--
-- @since 1.1.5
wrapConnection :: Sqlite.Connection -> LogFunc -> IO SqlBackend
wrapConnection = wrapConnectionInfo (mkSqliteConnectionInfo "")

-- | Retry if a Busy is thrown, following an exponential backoff strategy.
--
-- @since 2.9.3
retryOnBusy :: (MonadUnliftIO m, MonadLoggerIO m) => m a -> m a
retryOnBusy action =
  start $ take 20 $ delays 1000
  where
    delays x
      | x >= 1000000 = repeat x
      | otherwise = x : delays (x * 2)

    start [] = do
      $logWarn "Out of retry attempts"
      action
    start (x:xs) = do
      -- Using try instead of catch to avoid creating a stack overflow
      eres <- withRunInIO $ \run -> E.try $ run action
      case eres of
        Left (Sqlite.SqliteException { Sqlite.seError = Sqlite.ErrorBusy }) -> do
          $logWarn "Encountered an SQLITE_BUSY, going to retry..."
          liftIO $ threadDelay x
          start xs
        Left e -> liftIO $ E.throwIO e
        Right y -> return y

-- | Wait until some noop action on the database does not return an 'Sqlite.ErrorBusy'. See 'retryOnBusy'.
--
-- @since 2.9.3
waitForDatabase
    :: (MonadUnliftIO m, MonadLoggerIO m, BackendCompatible SqlBackend backend)
    => ReaderT backend m ()
waitForDatabase = retryOnBusy $ rawExecute "SELECT 42" []

-- | Wrap up a raw 'Sqlite.Connection' as a Persistent SQL
-- 'Connection', allowing full control over WAL and FK constraints.
--
-- @since 2.6.2
wrapConnectionInfo
    :: SqliteConnectionInfo
    -> Sqlite.Connection
    -> LogFunc
    -> IO SqlBackend
wrapConnectionInfo connInfo conn logFunc = do
    let
        -- Turn on the write-ahead log
        -- https://github.com/yesodweb/persistent/issues/363
        walPragma
          | _walEnabled connInfo = (("PRAGMA journal_mode=WAL;", True):)
          | otherwise = id

        -- Turn on foreign key constraints
        -- https://github.com/yesodweb/persistent/issues/646
        fkPragma
          | _fkEnabled connInfo = (("PRAGMA foreign_keys = on;", False):)
          | otherwise = id

        -- Allow arbitrary additional pragmas to be set
        -- https://github.com/commercialhaskell/stack/issues/4247
        pragmas = walPragma $ fkPragma $ map (, False) $ _extraPragmas connInfo

    forM_ pragmas $ \(pragma, shouldRetry) -> flip runLoggingT logFunc $
        (if shouldRetry then retryOnBusy else id) $ liftIO $ do
        stmt <- Sqlite.prepare conn pragma
        _ <- Sqlite.stepConn conn stmt
        Sqlite.reset conn stmt
        Sqlite.finalize stmt

    smap <- newIORef $ Map.empty
    return $
        setConnMaxParams 999 $
        setConnPutManySql putManySql $
        setConnRepsertManySql repsertManySql $
        mkSqlBackend MkSqlBackendArgs
            { connPrepare = prepare' conn
            , connStmtMap = smap
            , connInsertSql = insertSql'
            , connClose = Sqlite.close conn
            , connMigrateSql = migrate'
            , connBegin = \f _ -> helper "BEGIN" f
            , connCommit = helper "COMMIT"
            , connRollback = ignoreExceptions . helper "ROLLBACK"
            , connEscapeFieldName = escape . unFieldNameDB
            , connEscapeTableName = escape . unEntityNameDB . getEntityDBName
            , connEscapeRawName = escape
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
-- @since 1.1.4
runSqlite :: (MonadUnliftIO m)
          => Text -- ^ connection string
          -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -- ^ database action
          -> m a
runSqlite connstr = runResourceT
                  . runNoLoggingT
                  . withSqliteConn connstr
                  . runSqlConn

-- | A convenience helper which creates a new database connection and runs the
-- given block, handling @MonadResource@ and @MonadLogger@ requirements. Note
-- that all log messages are discarded.
--
-- @since 2.6.2
runSqliteInfo :: (MonadUnliftIO m)
              => SqliteConnectionInfo
              -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -- ^ database action
              -> m a
runSqliteInfo conInfo = runResourceT
                      . runNoLoggingT
                      . withSqliteConnInfo conInfo
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
    case getEntityId ent of
        EntityIdNaturalKey _ ->
          ISRManyKeys sql vals
            where sql = T.concat
                    [ "INSERT INTO "
                    , escapeE $ getEntityDBName ent
                    , "("
                    , T.intercalate "," $ map (escapeF . fieldDB) cols
                    , ") VALUES("
                    , T.intercalate "," (map (const "?") cols)
                    , ")"
                    ]
        EntityIdField fd ->
          ISRInsertGet ins sel
            where
              sel = T.concat
                  [ "SELECT "
                  , escapeF $ fieldDB fd
                  , " FROM "
                  , escapeE $ getEntityDBName ent
                  , " WHERE _ROWID_=last_insert_rowid()"
                  ]
              ins = T.concat
                  [ "INSERT INTO "
                  , escapeE $ getEntityDBName ent
                  , if null cols
                        then " VALUES(null)"
                        else T.concat
                          [ "("
                          , T.intercalate "," $ map (escapeF . fieldDB) $ cols
                          , ") VALUES("
                          , T.intercalate "," (map (const "?") cols)
                          , ")"
                          ]
                  ]
  where
    notGenerated =
        isNothing . fieldGenerated
    cols =
        filter notGenerated $ getEntityFields ent

execute' :: Sqlite.Connection -> Sqlite.Statement -> [PersistValue] -> IO Int64
execute' conn stmt vals = flip finally (liftIO $ Sqlite.reset conn stmt) $ do
    Sqlite.bind stmt vals
    _ <- Sqlite.stepConn conn stmt
    Sqlite.changes conn

withStmt'
          :: MonadIO m
          => Sqlite.Connection
          -> Sqlite.Statement
          -> [PersistValue]
          -> Acquire (ConduitM () [PersistValue] m ())
withStmt' conn stmt vals = do
    _ <- mkAcquire
        (Sqlite.bind stmt vals >> return stmt)
        (Sqlite.reset conn)
    return pull
  where
    pull = do
        x <- liftIO $ Sqlite.stepConn conn stmt
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
showSqlType (SqlOther t) = t

sqliteMkColumns :: [EntityDef] -> EntityDef -> ([Column], [UniqueDef], [ForeignDef])
sqliteMkColumns allDefs t = mkColumns allDefs t emptyBackendSpecificOverrides

migrate'
    :: [EntityDef]
    -> (Text -> IO Statement)
    -> EntityDef
    -> IO (Either [Text] [(Bool, Text)])
migrate' allDefs getter val = do
    let (cols, uniqs, fdefs) = sqliteMkColumns allDefs val
    let newSql = mkCreateTable False def (filter (not . safeToRemove val . cName) cols, uniqs, fdefs)
    stmt <- getter "SELECT sql FROM sqlite_master WHERE type='table' AND name=?"
    oldSql' <- with (stmtQuery stmt [PersistText $ unEntityNameDB table])
      (\src -> runConduit $ src .| go)
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
    table = getEntityDBName def
    go = do
        x <- CL.head
        case x of
            Nothing -> return Nothing
            Just [PersistText y] -> return $ Just y
            Just y -> error $ "Unexpected result from sqlite_master: " ++ show y

-- | Mock a migration even when the database is not present.
-- This function performs the same functionality of 'printMigration'
-- with the difference that an actual database isn't needed for it.
mockMigration :: Migration -> IO ()
mockMigration mig = do
    smap <- newIORef $ Map.empty
    let sqlbackend =
            setConnMaxParams 999 $
            mkSqlBackend MkSqlBackendArgs
                { connPrepare = \_ -> do
                    return Statement
                        { stmtFinalize = return ()
                        , stmtReset = return ()
                        , stmtExecute = undefined
                        , stmtQuery = \_ -> return $ return ()
                        }
                , connStmtMap = smap
                , connInsertSql = insertSql'
                , connClose = undefined
                , connMigrateSql = migrate'
                , connBegin = \f _ -> helper "BEGIN" f
                , connCommit = helper "COMMIT"
                , connRollback = ignoreExceptions . helper "ROLLBACK"
                , connEscapeFieldName = escape . unFieldNameDB
                , connEscapeTableName = escape . unEntityNameDB . getEntityDBName
                , connEscapeRawName = escape
                , connNoLimit = "LIMIT -1"
                , connRDBMS = "sqlite"
                , connLimitOffset = decorateSQLWithLimitOffset "LIMIT -1"
                , connLogFunc = undefined
                }
        result = runReaderT . runWriterT . runWriterT $ mig
    resp <- result sqlbackend
    mapM_ TIO.putStrLn $ map snd $ snd resp
  where
    helper t getter = do
        stmt <- getter t
        _ <- stmtExecute stmt []
        stmtReset stmt
    ignoreExceptions =
        E.handle (\(_ :: E.SomeException) -> return ())

-- | Check if a column name is listed as the "safe to remove" in the entity
-- list.
safeToRemove :: EntityDef -> FieldNameDB -> Bool
safeToRemove def (FieldNameDB colName)
    = any (elem FieldAttrSafeToRemove . fieldAttrs)
    $ filter ((== FieldNameDB colName) . fieldDB)
    $ allEntityFields
  where
    allEntityFields =
        getEntityFieldsDatabase def <> case getEntityId def of
            EntityIdField fdef ->
                [fdef]
            _ ->
                []

getCopyTable :: [EntityDef]
             -> (Text -> IO Statement)
             -> EntityDef
             -> IO [(Bool, Text)]
getCopyTable allDefs getter def = do
    stmt <- getter $ T.concat [ "PRAGMA table_info(", escapeE table, ")" ]
    oldCols' <- with (stmtQuery stmt []) (\src -> runConduit $ src .| getCols)
    let oldCols = map FieldNameDB oldCols'
    let newCols = filter (not . safeToRemove def) $ map cName cols
    let common = filter (`elem` oldCols) newCols
    return [ (False, tmpSql)
           , (False, copyToTemp common)
           , (common /= filter (not . safeToRemove def) oldCols, dropOld)
           , (False, newSql)
           , (False, copyToFinal newCols)
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
    table = getEntityDBName def
    tableTmp = EntityNameDB $ unEntityNameDB table <> "_backup"
    (cols, uniqs, fdef) = sqliteMkColumns allDefs def
    cols' = filter (not . safeToRemove def . cName) cols
    newSql = mkCreateTable False def (cols', uniqs, fdef)
    tmpSql = mkCreateTable True (setEntityDBName tableTmp def) (cols', uniqs, [])
    dropTmp = "DROP TABLE " <> escapeE tableTmp
    dropOld = "DROP TABLE " <> escapeE table
    copyToTemp common = T.concat
        [ "INSERT INTO "
        , escapeE tableTmp
        , "("
        , T.intercalate "," $ map escapeF common
        , ") SELECT "
        , T.intercalate "," $ map escapeF common
        , " FROM "
        , escapeE table
        ]
    copyToFinal newCols = T.concat
        [ "INSERT INTO "
        , escapeE table
        , " SELECT "
        , T.intercalate "," $ map escapeF newCols
        , " FROM "
        , escapeE tableTmp
        ]

mkCreateTable :: Bool -> EntityDef -> ([Column], [UniqueDef], [ForeignDef]) -> Text
mkCreateTable isTemp entity (cols, uniqs, fdefs) =
    T.concat (header <> columns <> footer)
  where
    header =
        [ "CREATE"
        , if isTemp then " TEMP" else ""
        , " TABLE "
        , escapeE $ getEntityDBName entity
        , "("
        ]

    footer =
        [ T.concat $ map sqlUnique uniqs
        , T.concat $ map sqlForeign fdefs
        , ")"
        ]

    columns = case getEntityId entity of
        EntityIdNaturalKey pdef ->
            [ T.drop 1 $ T.concat $ map (sqlColumn isTemp) cols
            , ", PRIMARY KEY "
            , "("
            , T.intercalate "," $ map (escapeF . fieldDB) $ toList $ compositeFields pdef
            , ")"
            ]

        EntityIdField fd ->
            [ escapeF $ fieldDB fd
            , " "
            , showSqlType $ fieldSqlType fd
            , " PRIMARY KEY"
            , mayDefault $ defaultAttribute $ fieldAttrs fd
            , T.concat $ map (sqlColumn isTemp) nonIdCols
            ]

    nonIdCols = filter (\c -> Just (cName c) /= fmap fieldDB (getEntityIdField entity)) cols

mayDefault :: Maybe Text -> Text
mayDefault def = case def of
    Nothing -> ""
    Just d -> " DEFAULT " <> d

mayGenerated :: Maybe Text -> Text
mayGenerated gen = case gen of
    Nothing -> ""
    Just g -> " GENERATED ALWAYS AS (" <> g <> ") STORED"

sqlColumn :: Bool -> Column -> Text
sqlColumn noRef (Column name isNull typ def gen _cn _maxLen ref) = T.concat
    [ ","
    , escapeF name
    , " "
    , showSqlType typ
    , if isNull then " NULL" else " NOT NULL"
    , mayDefault def
    , mayGenerated gen
    , case ref of
        Nothing -> ""
        Just ColumnReference {crTableName=table, crFieldCascade=cascadeOpts} ->
          if noRef then "" else " REFERENCES " <> escapeE table
            <> onDelete cascadeOpts <> onUpdate cascadeOpts
    ]
  where
    onDelete opts = maybe "" (T.append " ON DELETE " . renderCascadeAction) (fcOnDelete opts)
    onUpdate opts = maybe "" (T.append " ON UPDATE " . renderCascadeAction) (fcOnUpdate opts)

sqlForeign :: ForeignDef -> Text
sqlForeign fdef = T.concat $
    [ ", CONSTRAINT "
    , escapeC $ foreignConstraintNameDBName fdef
    , " FOREIGN KEY("
    , T.intercalate "," $ map (escapeF . snd. fst) $ foreignFields fdef
    , ") REFERENCES "
    , escapeE $ foreignRefTableDBName fdef
    , "("
    , T.intercalate "," $ map (escapeF . snd . snd) $ foreignFields fdef
    , ")"
    ] ++ onDelete ++ onUpdate
  where
    onDelete =
        fmap (T.append " ON DELETE ")
        $ showAction
        $ fcOnDelete
        $ foreignFieldCascade fdef
    onUpdate =
        fmap (T.append " ON UPDATE ")
        $ showAction
        $ fcOnUpdate
        $ foreignFieldCascade fdef

    showAction = maybeToList . fmap renderCascadeAction

sqlUnique :: UniqueDef -> Text
sqlUnique (UniqueDef _ cname cols _) = T.concat
    [ ",CONSTRAINT "
    , escapeC cname
    , " UNIQUE ("
    , T.intercalate "," $ map (escapeF . snd) $ toList cols
    , ")"
    ]

escapeC :: ConstraintNameDB -> Text
escapeC = escapeWith escape

escapeE :: EntityNameDB -> Text
escapeE = escapeWith escape

escapeF :: FieldNameDB -> Text
escapeF = escapeWith escape

escape :: Text -> Text
escape s =
    T.concat [q, T.concatMap go s, q]
  where
    q = T.singleton '"'
    go '"' = "\"\""
    go c = T.singleton c

putManySql :: EntityDef -> Int -> Text
putManySql ent n = putManySql' conflictColumns (toList fields) ent n
  where
    fields = getEntityFields ent
    conflictColumns = concatMap (map (escapeF . snd) . toList . uniqueFields) (getEntityUniques ent)

repsertManySql :: EntityDef -> Int -> Text
repsertManySql ent n = putManySql' conflictColumns (toList fields) ent n
  where
    fields = keyAndEntityFields ent
    conflictColumns = escapeF . fieldDB <$> toList (getEntityKeyFields ent)

putManySql' :: [Text] -> [FieldDef] -> EntityDef -> Int -> Text
putManySql' conflictColumns fields ent n = q
  where
    fieldDbToText = escapeF . fieldDB
    mkAssignment f = T.concat [f, "=EXCLUDED.", f]

    table = escapeE . getEntityDBName $ ent
    columns = Util.commaSeparated $ map fieldDbToText fields
    placeholders = map (const "?") fields
    updates = map (mkAssignment . fieldDbToText) fields

    q = T.concat
        [ "INSERT INTO "
        , table
        , Util.parenWrapped columns
        , " VALUES "
        , Util.commaSeparated . replicate n
            . Util.parenWrapped . Util.commaSeparated $ placeholders
        , " ON CONFLICT "
        , Util.parenWrapped . Util.commaSeparated $ conflictColumns
        , " DO UPDATE SET "
        , Util.commaSeparated updates
        ]

-- | Information required to setup a connection pool.
data SqliteConf = SqliteConf
    { sqlDatabase :: Text
    , sqlPoolSize :: Int
    }
    | SqliteConfInfo
    { sqlConnInfo :: SqliteConnectionInfo
    , sqlPoolSize :: Int
    } deriving Show

instance FromJSON SqliteConf where
    parseJSON v = modifyFailure ("Persistent: error loading Sqlite conf: " ++) $ flip (withObject "SqliteConf") v parser where
        parser o = if HashMap.member "database" o
                      then SqliteConf
                            <$> o .: "database"
                            <*> o .: "poolsize"
                      else SqliteConfInfo
                            <$> o .: "connInfo"
                            <*> o .: "poolsize"

instance PersistConfig SqliteConf where
    type PersistConfigBackend SqliteConf = SqlPersistT
    type PersistConfigPool SqliteConf = ConnectionPool
    createPoolConfig (SqliteConf cs size) = runNoLoggingT $ createSqlitePoolFromInfo (conStringToInfo cs) size -- FIXME
    createPoolConfig (SqliteConfInfo info size) = runNoLoggingT $ createSqlitePoolFromInfo info size -- FIXME
    runPool _ = runSqlPool
    loadConfig = parseJSON

finally :: MonadUnliftIO m
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was raised)
        -> m a
finally a sequel = withUnliftIO $ \u ->
                     E.finally (unliftIO u a)
                               (unliftIO u sequel)
{-# INLINABLE finally #-}
-- | Creates a SqliteConnectionInfo from a connection string, with the
-- default settings.
--
-- @since 2.6.2
mkSqliteConnectionInfo :: Text -> SqliteConnectionInfo
mkSqliteConnectionInfo fp = SqliteConnectionInfo fp True True []

-- | Parses connection options from a connection string. Used only to provide deprecated API.
conStringToInfo :: Text -> SqliteConnectionInfo
conStringToInfo connStr = SqliteConnectionInfo connStr' enableWal True [] where
    (connStr', enableWal) = case () of
        ()
            | Just cs <- T.stripPrefix "WAL=on "  connStr -> (cs, True)
            | Just cs <- T.stripPrefix "WAL=off " connStr -> (cs, False)
            | otherwise                                   -> (connStr, True)

-- | Information required to connect to a sqlite database. We export
-- lenses instead of fields to avoid being limited to the current
-- implementation.
--
-- @since 2.6.2
data SqliteConnectionInfo = SqliteConnectionInfo
    { _sqlConnectionStr :: Text -- ^ connection string for the database. Use @:memory:@ for an in-memory database.
    , _walEnabled :: Bool -- ^ if the write-ahead log is enabled - see https://github.com/yesodweb/persistent/issues/363.
    , _fkEnabled :: Bool -- ^ if foreign-key constraints are enabled.
    , _extraPragmas :: [Text] -- ^ additional pragmas to be set on initialization
    } deriving Show

instance FromJSON SqliteConnectionInfo where
    parseJSON v = modifyFailure ("Persistent: error loading SqliteConnectionInfo: " ++) $
      flip (withObject "SqliteConnectionInfo") v $ \o -> SqliteConnectionInfo
        <$> o .: "connectionString"
        <*> o .: "walEnabled"
        <*> o .: "fkEnabled"
        <*> o .:? "extraPragmas" .!= []

-- | Data type for reporting foreign key violations using 'checkForeignKeys'.
--
-- @since 2.11.1
data ForeignKeyViolation = ForeignKeyViolation
    { foreignKeyTable :: Text -- ^ The table of the violated constraint
    , foreignKeyColumn :: Text -- ^ The column of the violated constraint
    , foreignKeyRowId :: Int64 -- ^ The ROWID of the row with the violated foreign key constraint
    } deriving (Eq, Ord, Show)

-- | Outputs all (if any) the violated foreign key constraints in the database.
--
-- The main use is to validate that no foreign key constraints were
-- broken/corrupted by anyone operating on the database with foreign keys
-- disabled. See 'fkEnabled'.
--
-- @since 2.11.1
checkForeignKeys
    :: (MonadResource m, MonadReader env m, BackendCompatible SqlBackend env)
    => ConduitM () ForeignKeyViolation m ()
checkForeignKeys = rawQuery query [] .| C.mapM parse
  where
    parse l = case l of
        [ PersistInt64 rowid , PersistText table , PersistText column ] ->
            return ForeignKeyViolation
                { foreignKeyTable = table
                , foreignKeyColumn = column
                , foreignKeyRowId = rowid
                }
        _ -> liftIO . E.throwIO . PersistMarshalError $ mconcat
            [ "Unexpected result from foreign key check:\n", T.pack (show l) ]

    query = T.unlines
        [ "SELECT origin.rowid, origin.\"table\", group_concat(foreignkeys.\"from\")"
        , "FROM pragma_foreign_key_check() AS origin"
        , "INNER JOIN pragma_foreign_key_list(origin.\"table\") AS foreignkeys"
        , "ON origin.fkid = foreignkeys.id AND origin.parent = foreignkeys.\"table\""
        , "GROUP BY origin.rowid"
        ]

-- | Like `withSqliteConnInfo`, but exposes the internal `Sqlite.Connection`.
-- For power users who want to manually interact with SQLite's C API via
-- internals exposed by "Database.Sqlite.Internal"
--
-- @since 2.10.2
withRawSqliteConnInfo
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => SqliteConnectionInfo
    -> (RawSqlite SqlBackend -> m a)
    -> m a
withRawSqliteConnInfo connInfo f = do
    logFunc <- askLoggerIO
    withRunInIO $ \run -> E.bracket (openBackend logFunc) closeBackend $ run . f
  where
    openBackend = openWith RawSqlite connInfo
    closeBackend = close' . _persistentBackend

-- | Like `createSqlitePoolFromInfo`, but like `withRawSqliteConnInfo` it
-- exposes the internal `Sqlite.Connection`.
--
-- For power users who want to manually interact with SQLite's C API via
-- internals exposed by "Database.Sqlite.Internal". The callback can be used to
-- run arbitrary actions on the connection upon allocation from the pool.
--
-- @since 2.10.6
createRawSqlitePoolFromInfo
    :: (MonadLoggerIO m, MonadUnliftIO m)
    => SqliteConnectionInfo
    -> (RawSqlite SqlBackend -> m ())
    -- ^ An action that is run whenever a new `RawSqlite` connection is
    -- allocated in the pool. The main use of this function is to register
    -- custom functions with the SQLite connection upon creation.
    -> Int
    -> m (Pool (RawSqlite SqlBackend))
createRawSqlitePoolFromInfo connInfo f n = do
    runIO <- askRunInIO
    let createRawSqlite logFun = do
            result <- openWith RawSqlite connInfo logFun
            result <$ runIO (f result)

    createSqlPool createRawSqlite n

-- | Like `createRawSqlitePoolFromInfo`, but doesn't require a callback
-- operating on the connection.
--
-- @since 2.10.6
createRawSqlitePoolFromInfo_
    :: (MonadLoggerIO m, MonadUnliftIO m)
    => SqliteConnectionInfo -> Int -> m (Pool (RawSqlite SqlBackend))
createRawSqlitePoolFromInfo_ connInfo =
  createRawSqlitePoolFromInfo connInfo (const (return ()))

-- | Like `createSqlitePoolInfo`, but based on `createRawSqlitePoolFromInfo`.
--
-- @since 2.10.6
withRawSqlitePoolInfo
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => SqliteConnectionInfo
    -> (RawSqlite SqlBackend -> m ())
    -> Int -- ^ number of connections to open
    -> (Pool (RawSqlite SqlBackend) -> m a)
    -> m a
withRawSqlitePoolInfo connInfo f n work = do
    runIO <- askRunInIO
    let createRawSqlite logFun = do
            result <- openWith RawSqlite connInfo logFun
            result <$ runIO (f result)

    withSqlPool createRawSqlite n work

-- | Like `createSqlitePoolInfo`, but based on `createRawSqlitePoolFromInfo_`.
--
-- @since 2.10.6
withRawSqlitePoolInfo_
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => SqliteConnectionInfo
    -> Int -- ^ number of connections to open
    -> (Pool (RawSqlite SqlBackend) -> m a)
    -> m a
withRawSqlitePoolInfo_ connInfo =
  withRawSqlitePoolInfo connInfo (const (return ()))

-- | Wrapper for persistent SqlBackends that carry the corresponding
-- `Sqlite.Connection`.
--
-- @since 2.10.2
data RawSqlite backend = RawSqlite
    { _persistentBackend :: backend -- ^ The persistent backend
    , _rawSqliteConnection :: Sqlite.Connection -- ^ The underlying `Sqlite.Connection`
    }

instance BackendCompatible b (RawSqlite b) where
    projectBackend = _persistentBackend

#if MIN_VERSION_base(4,12,0)
instance (PersistCore b) => PersistCore (RawSqlite b) where
  newtype BackendKey (RawSqlite b) = RawSqliteKey { unRawSqliteKey :: BackendKey (Compatible b (RawSqlite b)) }

makeCompatibleKeyInstances [t| forall b. Compatible b (RawSqlite b) |]
#else
instance (PersistCore b) => PersistCore (RawSqlite b) where
  newtype BackendKey (RawSqlite b) = RawSqliteKey { unRawSqliteKey :: BackendKey (RawSqlite b) }

deriving instance (Show (BackendKey b)) => Show (BackendKey (RawSqlite b))
deriving instance (Read (BackendKey b)) => Read (BackendKey (RawSqlite b))
deriving instance (Eq (BackendKey b)) => Eq (BackendKey (RawSqlite b))
deriving instance (Ord (BackendKey b)) => Ord (BackendKey (RawSqlite b))
deriving instance (Num (BackendKey b)) => Num (BackendKey (RawSqlite b))
deriving instance (Integral (BackendKey b)) => Integral (BackendKey (RawSqlite b))
deriving instance (PersistField (BackendKey b)) => PersistField (BackendKey (RawSqlite b))
deriving instance (PersistFieldSql (BackendKey b)) => PersistFieldSql (BackendKey (RawSqlite b))
deriving instance (Real (BackendKey b)) => Real (BackendKey (RawSqlite b))
deriving instance (Enum (BackendKey b)) => Enum (BackendKey (RawSqlite b))
deriving instance (Bounded (BackendKey b)) => Bounded (BackendKey (RawSqlite b))
deriving instance (ToJSON (BackendKey b)) => ToJSON (BackendKey (RawSqlite b))
deriving instance (FromJSON (BackendKey b)) => FromJSON (BackendKey (RawSqlite b))
#endif


#if MIN_VERSION_base(4,12,0)
$(pure [])

makeCompatibleInstances [t| forall b. Compatible b (RawSqlite b) |]
#else
instance HasPersistBackend b => HasPersistBackend (RawSqlite b) where
    type BaseBackend (RawSqlite b) = BaseBackend b
    persistBackend = persistBackend . _persistentBackend

instance (PersistStoreRead b) => PersistStoreRead (RawSqlite b) where
    get = withReaderT _persistentBackend . get
    getMany = withReaderT _persistentBackend . getMany

instance (PersistQueryRead b) => PersistQueryRead (RawSqlite b) where
    selectSourceRes filts opts = withReaderT _persistentBackend $ selectSourceRes filts opts
    selectFirst filts opts = withReaderT _persistentBackend $ selectFirst filts opts
    selectKeysRes filts opts = withReaderT _persistentBackend $ selectKeysRes filts opts
    count = withReaderT _persistentBackend . count
    exists = withReaderT _persistentBackend . exists

instance (PersistQueryWrite b) => PersistQueryWrite (RawSqlite b) where
    updateWhere filts updates = withReaderT _persistentBackend $ updateWhere filts updates
    deleteWhere = withReaderT _persistentBackend . deleteWhere

instance (PersistUniqueRead b) => PersistUniqueRead (RawSqlite b) where
    getBy = withReaderT _persistentBackend . getBy

instance (PersistStoreWrite b) => PersistStoreWrite (RawSqlite b) where
    insert = withReaderT _persistentBackend . insert
    insert_ = withReaderT _persistentBackend . insert_
    insertMany = withReaderT _persistentBackend . insertMany
    insertMany_ = withReaderT _persistentBackend . insertMany_
    insertEntityMany = withReaderT _persistentBackend . insertEntityMany
    insertKey k = withReaderT _persistentBackend . insertKey k
    repsert k = withReaderT _persistentBackend . repsert k
    repsertMany = withReaderT _persistentBackend . repsertMany
    replace k = withReaderT _persistentBackend . replace k
    delete = withReaderT _persistentBackend . delete
    update k = withReaderT _persistentBackend . update k
    updateGet k = withReaderT _persistentBackend . updateGet k

instance (PersistUniqueWrite b) => PersistUniqueWrite (RawSqlite b) where
    deleteBy = withReaderT _persistentBackend . deleteBy
    insertUnique = withReaderT _persistentBackend . insertUnique
    upsert rec = withReaderT _persistentBackend . upsert rec
    upsertBy uniq rec = withReaderT _persistentBackend . upsertBy uniq rec
    putMany = withReaderT _persistentBackend . putMany
#endif

makeLenses ''RawSqlite
makeLenses ''SqliteConnectionInfo
