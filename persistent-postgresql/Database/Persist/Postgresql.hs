{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

-- | A postgresql backend for persistent.
module Database.Persist.Postgresql
    ( withPostgresqlPool
    , withPostgresqlPoolWithVersion
    , withPostgresqlPoolWithConf
    , withPostgresqlPoolModified
    , withPostgresqlPoolModifiedWithVersion
    , withPostgresqlConn
    , withPostgresqlConnWithVersion
    , createPostgresqlPool
    , createPostgresqlPoolModified
    , createPostgresqlPoolModifiedWithVersion
    , createPostgresqlPoolTailored
    , createPostgresqlPoolWithConf
    , module Database.Persist.Sql
    , ConnectionString
    , HandleUpdateCollision
    , copyField
    , copyUnlessNull
    , copyUnlessEmpty
    , copyUnlessEq
    , excludeNotEqualToOriginal
    , PostgresConf (..)
    , PgInterval (..)
    , upsertWhere
    , upsertManyWhere
    , openSimpleConn
    , openSimpleConnWithVersion
    , getServerVersion
    , getSimpleConn
    , tableName
    , fieldName
    , mockMigration
    , migrateEnableExtension
    , PostgresConfHooks (..)
    , defaultPostgresConfHooks
    , RawPostgresql (..)
    , createRawPostgresqlPool
    , createRawPostgresqlPoolModified
    , createRawPostgresqlPoolModifiedWithVersion
    , createRawPostgresqlPoolWithConf
    , createBackend
    ) where

import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Internal as PG
import Database.PostgreSQL.Simple.Ok (Ok (..))
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Database.PostgreSQL.Simple.Types as PG

import Control.Exception (Exception, throw, throwIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT (..), asks, runReaderT)
#if !MIN_VERSION_base(4,12,0)
import Control.Monad.Trans.Reader (withReaderT)
#endif
import Control.Monad.Trans.Writer (WriterT (..), runWriterT)
import qualified Data.List.NonEmpty as NEL
import Data.Proxy (Proxy (..))

import Data.Acquire (Acquire, mkAcquire)
import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import qualified Data.Attoparsec.Text as AT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Conduit
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.IORef
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text.Read (rational)
import System.Environment (getEnvironment)

#if MIN_VERSION_base(4,12,0)
import Database.Persist.Compatible
#endif
import qualified Data.Vault.Strict as Vault
import Database.Persist.Postgresql.Internal
import Database.Persist.Sql
import qualified Database.Persist.Sql.Util as Util
import Database.Persist.SqlBackend
import System.IO.Unsafe (unsafePerformIO)

-- | A @libpq@ connection string.  A simple example of connection
-- string would be @\"host=localhost port=5432 user=test
-- dbname=test password=test\"@.  Please read libpq's
-- documentation at
-- <https://www.postgresql.org/docs/current/static/libpq-connect.html>
-- for more details on how to create such strings.
type ConnectionString = ByteString

-- | PostgresServerVersionError exception. This is thrown when persistent
-- is unable to find the version of the postgreSQL server.
data PostgresServerVersionError = PostgresServerVersionError String

instance Show PostgresServerVersionError where
    show (PostgresServerVersionError uniqueMsg) =
        "Unexpected PostgreSQL server version, got " <> uniqueMsg
instance Exception PostgresServerVersionError

-- | Create a PostgreSQL connection pool and run the given action. The pool is
-- properly released after the action finishes using it.  Note that you should
-- not use the given 'ConnectionPool' outside the action since it may already
-- have been released.
-- The provided action should use 'runSqlConn' and *not* 'runReaderT' because
-- the former brackets the database action with transaction begin/commit.
withPostgresqlPool
    :: (MonadLoggerIO m, MonadUnliftIO m)
    => ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in
    -- the pool.
    -> (Pool SqlBackend -> m a)
    -- ^ Action to be executed that uses the
    -- connection pool.
    -> m a
withPostgresqlPool ci = withPostgresqlPoolWithVersion getServerVersion ci

-- | Same as 'withPostgresPool', but takes a callback for obtaining
-- the server version (to work around an Amazon Redshift bug).
--
-- @since 2.6.2
withPostgresqlPoolWithVersion
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO (Maybe Double))
    -- ^ Action to perform to get the server version.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in
    -- the pool.
    -> (Pool SqlBackend -> m a)
    -- ^ Action to be executed that uses the
    -- connection pool.
    -> m a
withPostgresqlPoolWithVersion getVerDouble ci = do
    let
        getVer = oldGetVersionToNew getVerDouble
    withSqlPool $ open' (const $ return ()) getVer id ci

-- | Same as 'withPostgresqlPool', but can be configured with 'PostgresConf' and 'PostgresConfHooks'.
--
-- @since 2.11.0.0
withPostgresqlPoolWithConf
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => PostgresConf
    -- ^ Configuration for connecting to Postgres
    -> PostgresConfHooks
    -- ^ Record of callback functions
    -> (Pool SqlBackend -> m a)
    -- ^ Action to be executed that uses the
    -- connection pool.
    -> m a
withPostgresqlPoolWithConf conf hooks = do
    let
        getVer = pgConfHooksGetServerVersion hooks
        modConn = pgConfHooksAfterCreate hooks
    let
        logFuncToBackend = open' modConn getVer id (pgConnStr conf)
    withSqlPoolWithConfig logFuncToBackend (postgresConfToConnectionPoolConfig conf)

-- | Same as 'withPostgresqlPool', but with the 'createPostgresqlPoolModified'
-- feature.
--
-- @since 2.13.5.0
withPostgresqlPoolModified
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO ())
    -- ^ Action to perform after connection is created.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in the pool.
    -> (Pool SqlBackend -> m t)
    -> m t
withPostgresqlPoolModified = withPostgresqlPoolModifiedWithVersion getServerVersion

-- | Same as 'withPostgresqlPool', but with the
-- 'createPostgresqlPoolModifiedWithVersion' feature.
--
-- @since 2.13.5.0
withPostgresqlPoolModifiedWithVersion
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO (Maybe Double))
    -- ^ Action to perform to get the server version.
    -> (PG.Connection -> IO ())
    -- ^ Action to perform after connection is created.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in the pool.
    -> (Pool SqlBackend -> m t)
    -> m t
withPostgresqlPoolModifiedWithVersion getVerDouble modConn ci = do
    withSqlPool (open' modConn (oldGetVersionToNew getVerDouble) id ci)

-- | Create a PostgreSQL connection pool.  Note that it's your
-- responsibility to properly close the connection pool when
-- unneeded.  Use 'withPostgresqlPool' for an automatic resource
-- control.
createPostgresqlPool
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open
    -- in the pool.
    -> m (Pool SqlBackend)
createPostgresqlPool = createPostgresqlPoolModified (const $ return ())

-- | Same as 'createPostgresqlPool', but additionally takes a callback function
-- for some connection-specific tweaking to be performed after connection
-- creation. This could be used, for example, to change the schema. For more
-- information, see:
--
-- <https://groups.google.com/d/msg/yesodweb/qUXrEN_swEo/O0pFwqwQIdcJ>
--
-- @since 2.1.3
createPostgresqlPoolModified
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO ())
    -- ^ Action to perform after connection is created.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in the pool.
    -> m (Pool SqlBackend)
createPostgresqlPoolModified = createPostgresqlPoolModifiedWithVersion getServerVersion

-- | Same as other similarly-named functions in this module, but takes callbacks for obtaining
-- the server version (to work around an Amazon Redshift bug) and connection-specific tweaking
-- (to change the schema).
--
-- @since 2.6.2
createPostgresqlPoolModifiedWithVersion
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO (Maybe Double))
    -- ^ Action to perform to get the server version.
    -> (PG.Connection -> IO ())
    -- ^ Action to perform after connection is created.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in the pool.
    -> m (Pool SqlBackend)
createPostgresqlPoolModifiedWithVersion = createPostgresqlPoolTailored open'

-- | Same as 'createPostgresqlPoolModifiedWithVersion', but takes a custom connection-creation
-- function.
--
-- The only time you should reach for this function is if you need to write custom logic for creating
-- a connection to the database.
--
-- @since 2.13.6
createPostgresqlPoolTailored
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => ( (PG.Connection -> IO ())
         -> (PG.Connection -> IO (NonEmpty Word))
         -> ((PG.Connection -> SqlBackend) -> PG.Connection -> SqlBackend)
         -> ConnectionString
         -> LogFunc
         -> IO SqlBackend
       )
    -- ^ Action that creates a postgresql connection (please see documentation on the un-exported @open'@ function in this same module.
    -> (PG.Connection -> IO (Maybe Double))
    -- ^ Action to perform to get the server version.
    -> (PG.Connection -> IO ())
    -- ^ Action to perform after connection is created.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in the pool.
    -> m (Pool SqlBackend)
createPostgresqlPoolTailored createConnection getVerDouble modConn ci = do
    let
        getVer = oldGetVersionToNew getVerDouble
    createSqlPool $ createConnection modConn getVer id ci

-- | Same as 'createPostgresqlPool', but can be configured with 'PostgresConf' and 'PostgresConfHooks'.
--
-- @since 2.11.0.0
createPostgresqlPoolWithConf
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => PostgresConf
    -- ^ Configuration for connecting to Postgres
    -> PostgresConfHooks
    -- ^ Record of callback functions
    -> m (Pool SqlBackend)
createPostgresqlPoolWithConf conf hooks = do
    let
        getVer = pgConfHooksGetServerVersion hooks
        modConn = pgConfHooksAfterCreate hooks
    createSqlPoolWithConfig
        (open' modConn getVer id (pgConnStr conf))
        (postgresConfToConnectionPoolConfig conf)

postgresConfToConnectionPoolConfig :: PostgresConf -> ConnectionPoolConfig
postgresConfToConnectionPoolConfig conf =
    ConnectionPoolConfig
        { connectionPoolConfigStripes = pgPoolStripes conf
        , connectionPoolConfigIdleTimeout = fromInteger $ pgPoolIdleTimeout conf
        , connectionPoolConfigSize = pgPoolSize conf
        }

-- | Same as 'withPostgresqlPool', but instead of opening a pool
-- of connections, only one connection is opened.
-- The provided action should use 'runSqlConn' and *not* 'runReaderT' because
-- the former brackets the database action with transaction begin/commit.
withPostgresqlConn
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => ConnectionString
    -> (SqlBackend -> m a)
    -> m a
withPostgresqlConn = withPostgresqlConnWithVersion getServerVersion

-- | Same as 'withPostgresqlConn', but takes a callback for obtaining
-- the server version (to work around an Amazon Redshift bug).
--
-- @since 2.6.2
withPostgresqlConnWithVersion
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO (Maybe Double))
    -> ConnectionString
    -> (SqlBackend -> m a)
    -> m a
withPostgresqlConnWithVersion getVerDouble = do
    let
        getVer = oldGetVersionToNew getVerDouble
    withSqlConn . open' (const $ return ()) getVer id

open'
    :: (PG.Connection -> IO ())
    -> (PG.Connection -> IO (NonEmpty Word))
    -> ((PG.Connection -> SqlBackend) -> PG.Connection -> backend)
    -- ^ How to construct the actual backend type desired. For most uses,
    -- this is just 'id', since the desired backend type is 'SqlBackend'.
    -- But some callers want a @'RawPostgresql' 'SqlBackend'@, and will
    -- pass in 'withRawConnection'.
    -> ConnectionString
    -> LogFunc
    -> IO backend
open' modConn getVer constructor cstr logFunc = do
    conn <- PG.connectPostgreSQL cstr
    modConn conn
    ver <- getVer conn
    smap <- newIORef mempty
    return $ constructor (createBackend logFunc ver smap) conn

-- | Gets the PostgreSQL server version
--
-- @since 2.13.6
getServerVersion :: PG.Connection -> IO (Maybe Double)
getServerVersion conn = do
    [PG.Only version] <- PG.query_ conn "show server_version"
    let
        version' = rational version
    --- λ> rational "9.8.3"
    --- Right (9.8,".3")
    --- λ> rational "9.8.3.5"
    --- Right (9.8,".3.5")
    case version' of
        Right (a, _) -> return $ Just a
        Left err -> throwIO $ PostgresServerVersionError err

getServerVersionNonEmpty :: PG.Connection -> IO (NonEmpty Word)
getServerVersionNonEmpty conn = do
    [PG.Only version] <- PG.query_ conn "show server_version"
    case AT.parseOnly parseVersion (T.pack version) of
        Left err ->
            throwIO $
                PostgresServerVersionError $
                    "Parse failure on: " <> version <> ". Error: " <> err
        Right versionComponents -> case NEL.nonEmpty versionComponents of
            Nothing ->
                throwIO $
                    PostgresServerVersionError $
                        "Empty Postgres version string: " <> version
            Just neVersion -> pure neVersion
  where
    -- Partially copied from the `versions` package
    -- Typically server_version gives e.g. 12.3
    -- In Persistent's CI, we get "12.4 (Debian 12.4-1.pgdg100+1)", so we ignore the trailing data.
    parseVersion = AT.decimal `AT.sepBy` AT.char '.'

-- | Choose upsert sql generation function based on postgresql version.
-- PostgreSQL version >= 9.5 supports native upsert feature,
-- so depending upon that we have to choose how the sql query is generated.
-- upsertFunction :: Double -> Maybe (EntityDef -> Text -> Text)
upsertFunction :: a -> NonEmpty Word -> Maybe a
upsertFunction f version =
    if (version >= postgres9dot5)
        then Just f
        else Nothing
  where

postgres9dot5 :: NonEmpty Word
postgres9dot5 = 9 NEL.:| [5]

-- | If the user doesn't supply a Postgres version, we assume this version.
--
-- This is currently below any version-specific features Persistent uses.
minimumPostgresVersion :: NonEmpty Word
minimumPostgresVersion = 9 NEL.:| [4]

oldGetVersionToNew
    :: (PG.Connection -> IO (Maybe Double)) -> (PG.Connection -> IO (NonEmpty Word))
oldGetVersionToNew oldFn = \conn -> do
    mDouble <- oldFn conn
    case mDouble of
        Nothing -> pure minimumPostgresVersion
        Just double -> do
            let
                (major, minor) = properFraction double
            pure $ major NEL.:| [floor minor]

-- | Generate a 'SqlBackend' from a 'PG.Connection'.
openSimpleConn :: LogFunc -> PG.Connection -> IO SqlBackend
openSimpleConn = openSimpleConnWithVersion getServerVersion

-- | Generate a 'SqlBackend' from a 'PG.Connection', but takes a callback for
-- obtaining the server version.
--
-- @since 2.9.1
openSimpleConnWithVersion
    :: (PG.Connection -> IO (Maybe Double))
    -> LogFunc
    -> PG.Connection
    -> IO SqlBackend
openSimpleConnWithVersion getVerDouble logFunc conn = do
    smap <- newIORef mempty
    serverVersion <- oldGetVersionToNew getVerDouble conn
    return $ createBackend logFunc serverVersion smap conn

underlyingConnectionKey :: Vault.Key PG.Connection
underlyingConnectionKey = unsafePerformIO Vault.newKey
{-# NOINLINE underlyingConnectionKey #-}

-- | Access underlying connection, returning 'Nothing' if the 'SqlBackend'
-- provided isn't backed by postgresql-simple.
--
-- @since 2.13.0
getSimpleConn
    :: (BackendCompatible SqlBackend backend) => backend -> Maybe PG.Connection
getSimpleConn = Vault.lookup underlyingConnectionKey <$> getConnVault

-- | Create the backend given a logging function, server version, mutable statement cell,
-- and connection.
--
-- @since 2.13.6
createBackend
    :: LogFunc
    -> NonEmpty Word
    -> IORef (Map.Map Text Statement)
    -> PG.Connection
    -> SqlBackend
createBackend logFunc serverVersion smap conn =
    maybe id setConnPutManySql (upsertFunction putManySql serverVersion) $
        maybe id setConnUpsertSql (upsertFunction upsertSql' serverVersion) $
            setConnInsertManySql insertManySql' $
                maybe id setConnRepsertManySql (upsertFunction repsertManySql serverVersion) $
                    modifyConnVault (Vault.insert underlyingConnectionKey conn) $
                        mkSqlBackend
                            MkSqlBackendArgs
                                { connPrepare = prepare' conn
                                , connStmtMap = smap
                                , connInsertSql = insertSql'
                                , connClose = PG.close conn
                                , connMigrateSql = migrate'
                                , connBegin = \_ mIsolation -> case mIsolation of
                                    Nothing -> PG.begin conn
                                    Just iso ->
                                        PG.beginLevel
                                            ( case iso of
                                                ReadUncommitted -> PG.ReadCommitted -- PG Upgrades uncommitted reads to committed anyways
                                                ReadCommitted -> PG.ReadCommitted
                                                RepeatableRead -> PG.RepeatableRead
                                                Serializable -> PG.Serializable
                                            )
                                            conn
                                , connCommit = const $ PG.commit conn
                                , connRollback = const $ PG.rollback conn
                                , connEscapeFieldName = escapeF
                                , connEscapeTableName = escapeE . getEntityDBName
                                , connEscapeRawName = escape
                                , connNoLimit = "LIMIT ALL"
                                , connRDBMS = "postgresql"
                                , connLimitOffset = decorateSQLWithLimitOffset "LIMIT ALL"
                                , connLogFunc = logFunc
                                }

prepare' :: PG.Connection -> Text -> IO Statement
prepare' conn sql = do
    let
        query = PG.Query (T.encodeUtf8 sql)
    return
        Statement
            { stmtFinalize = return ()
            , stmtReset = return ()
            , stmtExecute = execute' conn query
            , stmtQuery = withStmt' conn query
            }

insertSql' :: EntityDef -> [PersistValue] -> InsertSqlResult
insertSql' ent vals =
    case getEntityId ent of
        EntityIdNaturalKey _pdef ->
            ISRManyKeys sql vals
        EntityIdField field ->
            ISRSingle (sql <> " RETURNING " <> escapeF (fieldDB field))
  where
    (fieldNames, placeholders) = unzip (Util.mkInsertPlaceholders ent escapeF)
    sql =
        T.concat
            [ "INSERT INTO "
            , escapeE $ getEntityDBName ent
            , if null (getEntityFields ent)
                then " DEFAULT VALUES"
                else
                    T.concat
                        [ "("
                        , T.intercalate "," fieldNames
                        , ") VALUES("
                        , T.intercalate "," placeholders
                        , ")"
                        ]
            ]

upsertSql' :: EntityDef -> NonEmpty (FieldNameHS, FieldNameDB) -> Text -> Text
upsertSql' ent uniqs updateVal =
    T.concat
        [ "INSERT INTO "
        , escapeE (getEntityDBName ent)
        , "("
        , T.intercalate "," fieldNames
        , ") VALUES ("
        , T.intercalate "," placeholders
        , ") ON CONFLICT ("
        , T.intercalate "," $ map (escapeF . snd) (NEL.toList uniqs)
        , ") DO UPDATE SET "
        , updateVal
        , " WHERE "
        , wher
        , " RETURNING ??"
        ]
  where
    (fieldNames, placeholders) = unzip (Util.mkInsertPlaceholders ent escapeF)

    wher = T.intercalate " AND " $ map (singleClause . snd) $ NEL.toList uniqs

    singleClause :: FieldNameDB -> Text
    singleClause field = escapeE (getEntityDBName ent) <> "." <> (escapeF field) <> " =?"

-- | SQL for inserting multiple rows at once and returning their primary keys.
insertManySql' :: EntityDef -> [[PersistValue]] -> InsertSqlResult
insertManySql' ent valss =
    ISRSingle sql
  where
    (fieldNames, placeholders) = unzip (Util.mkInsertPlaceholders ent escapeF)
    sql =
        T.concat
            [ "INSERT INTO "
            , escapeE (getEntityDBName ent)
            , "("
            , T.intercalate "," fieldNames
            , ") VALUES ("
            , T.intercalate "),(" $ replicate (length valss) $ T.intercalate "," placeholders
            , ") RETURNING "
            , Util.commaSeparated $ NEL.toList $ Util.dbIdColumnsEsc escapeF ent
            ]

execute' :: PG.Connection -> PG.Query -> [PersistValue] -> IO Int64
execute' conn query vals = PG.execute conn query (map P vals)

withStmt'
    :: (MonadIO m)
    => PG.Connection
    -> PG.Query
    -> [PersistValue]
    -> Acquire (ConduitM () [PersistValue] m ())
withStmt' conn query vals =
    pull `fmap` mkAcquire openS closeS
  where
    openS = do
        -- Construct raw query
        rawquery <- PG.formatQuery conn query (map P vals)

        -- Take raw connection
        (rt, rr, rc, ids) <- PG.withConnection conn $ \rawconn -> do
            -- Execute query
            mret <- LibPQ.exec rawconn rawquery
            case mret of
                Nothing -> do
                    merr <- LibPQ.errorMessage rawconn
                    fail $ case merr of
                        Nothing -> "Postgresql.withStmt': unknown error"
                        Just e -> "Postgresql.withStmt': " ++ B8.unpack e
                Just ret -> do
                    -- Check result status
                    status <- LibPQ.resultStatus ret
                    case status of
                        LibPQ.TuplesOk -> return ()
                        _ -> PG.throwResultError "Postgresql.withStmt': bad result status " ret status

                    -- Get number and type of columns
                    cols <- LibPQ.nfields ret
                    oids <- forM [0 .. cols - 1] $ \col -> fmap ((,) col) (LibPQ.ftype ret col)
                    -- Ready to go!
                    rowRef <- newIORef (LibPQ.Row 0)
                    rowCount <- LibPQ.ntuples ret
                    return (ret, rowRef, rowCount, oids)
        let
            getters =
                map (\(col, oid) -> getGetter oid $ PG.Field rt col oid) ids
        return (rt, rr, rc, getters)

    closeS (ret, _, _, _) = LibPQ.unsafeFreeResult ret

    pull x = do
        y <- liftIO $ pullS x
        case y of
            Nothing -> return ()
            Just z -> yield z >> pull x

    pullS (ret, rowRef, rowCount, getters) = do
        row <- atomicModifyIORef rowRef (\r -> (r + 1, r))
        if row == rowCount
            then return Nothing
            else fmap Just $ forM (zip getters [0 ..]) $ \(getter, col) -> do
                mbs <- LibPQ.getvalue' ret row col
                case mbs of
                    Nothing ->
                        -- getvalue' verified that the value is NULL.
                        -- However, that does not mean that there are
                        -- no NULL values inside the value (e.g., if
                        -- we're dealing with an array of optional values).
                        return PersistNull
                    Just bs -> do
                        ok <- PGFF.runConversion (getter mbs) conn
                        bs `seq` case ok of
                            Errors (exc : _) -> throw exc
                            Errors [] -> error "Got an Errors, but no exceptions"
                            Ok v -> return v

migrate'
    :: [EntityDef]
    -> (Text -> IO Statement)
    -> EntityDef
    -> IO (Either [Text] CautiousMigration)
migrate' allDefs getter entity = fmap (fmap $ map showAlterDb) $ migrateStructured allDefs getter entity

-- | Get the SQL string for the table that a PersistEntity represents.
-- Useful for raw SQL queries.
tableName :: (PersistEntity record) => record -> Text
tableName = escapeE . tableDBName

-- | Get the SQL string for the field that an EntityField represents.
-- Useful for raw SQL queries.
fieldName :: (PersistEntity record) => EntityField record typ -> Text
fieldName = escapeF . fieldDBName

-- | Information required to connect to a PostgreSQL database
-- using @persistent@'s generic facilities.  These values are the
-- same that are given to 'withPostgresqlPool'.
data PostgresConf = PostgresConf
    { pgConnStr :: ConnectionString
    -- ^ The connection string.
    , -- TODO: Currently stripes, idle timeout, and pool size are all separate fields
      -- When Persistent next does a large breaking release (3.0?), we should consider making these just a single ConnectionPoolConfig value
      --
      -- Currently there the idle timeout is an Integer, rather than resource-pool's NominalDiffTime type.
      -- This is because the time package only recently added the Read instance for NominalDiffTime.
      -- Future TODO: Consider removing the Read instance, and/or making the idle timeout a NominalDiffTime.

      pgPoolStripes :: Int
    -- ^ How many stripes to divide the pool into. See "Data.Pool" for details.
    -- @since 2.11.0.0
    , pgPoolIdleTimeout :: Integer -- Ideally this would be a NominalDiffTime, but that type lacks a Read instance https://github.com/haskell/time/issues/130

    -- ^ How long connections can remain idle before being disposed of, in seconds.
    -- @since 2.11.0.0
    , pgPoolSize :: Int
    -- ^ How many connections should be held in the connection pool.
    }
    deriving (Show, Read, Data)

instance FromJSON PostgresConf where
    parseJSON v = modifyFailure ("Persistent: error loading PostgreSQL conf: " ++) $
        flip (withObject "PostgresConf") v $ \o -> do
            let
                defaultPoolConfig = defaultConnectionPoolConfig
            database <- o .: "database"
            host <- o .: "host"
            port <- o .:? "port" .!= 5432
            user <- o .: "user"
            password <- o .: "password"
            poolSize <- o .:? "poolsize" .!= (connectionPoolConfigSize defaultPoolConfig)
            poolStripes <-
                o .:? "stripes" .!= (connectionPoolConfigStripes defaultPoolConfig)
            poolIdleTimeout <-
                o
                    .:? "idleTimeout"
                    .!= (floor $ connectionPoolConfigIdleTimeout defaultPoolConfig)
            let
                ci =
                    PG.ConnectInfo
                        { PG.connectHost = host
                        , PG.connectPort = port
                        , PG.connectUser = user
                        , PG.connectPassword = password
                        , PG.connectDatabase = database
                        }
                cstr = PG.postgreSQLConnectionString ci
            return $ PostgresConf cstr poolStripes poolIdleTimeout poolSize
instance PersistConfig PostgresConf where
    type PersistConfigBackend PostgresConf = SqlPersistT
    type PersistConfigPool PostgresConf = ConnectionPool
    createPoolConfig conf = runNoLoggingT $ createPostgresqlPoolWithConf conf defaultPostgresConfHooks
    runPool _ = runSqlPool
    loadConfig = parseJSON

    applyEnv c0 = do
        env <- getEnvironment
        return $
            addUser env $
                addPass env $
                    addDatabase env $
                        addPort env $
                            addHost env c0
      where
        addParam param val c =
            c{pgConnStr = B8.concat [pgConnStr c, " ", param, "='", pgescape val, "'"]}

        pgescape = B8.pack . go
          where
            go ('\'' : rest) = '\\' : '\'' : go rest
            go ('\\' : rest) = '\\' : '\\' : go rest
            go (x : rest) = x : go rest
            go [] = []

        maybeAddParam param envvar env =
            maybe id (addParam param) $
                lookup envvar env

        addHost = maybeAddParam "host" "PGHOST"
        addPort = maybeAddParam "port" "PGPORT"
        addUser = maybeAddParam "user" "PGUSER"
        addPass = maybeAddParam "password" "PGPASS"
        addDatabase = maybeAddParam "dbname" "PGDATABASE"

-- | Hooks for configuring the Persistent/its connection to Postgres
--
-- @since 2.11.0
data PostgresConfHooks = PostgresConfHooks
    { pgConfHooksGetServerVersion :: PG.Connection -> IO (NonEmpty Word)
    -- ^ Function to get the version of Postgres
    --
    -- The default implementation queries the server with "show server_version".
    -- Some variants of Postgres, such as Redshift, don't support showing the version.
    -- It's recommended you return a hardcoded version in those cases.
    --
    -- @since 2.11.0
    , pgConfHooksAfterCreate :: PG.Connection -> IO ()
    -- ^ Action to perform after a connection is created.
    --
    -- Typical uses of this are modifying the connection (e.g. to set the schema) or logging a connection being created.
    --
    -- The default implementation does nothing.
    --
    -- @since 2.11.0
    }

-- | Default settings for 'PostgresConfHooks'. See the individual fields of 'PostgresConfHooks' for the default values.
--
-- @since 2.11.0
defaultPostgresConfHooks :: PostgresConfHooks
defaultPostgresConfHooks =
    PostgresConfHooks
        { pgConfHooksGetServerVersion = getServerVersionNonEmpty
        , pgConfHooksAfterCreate = const $ pure ()
        }

mockMigrate
    :: [EntityDef]
    -> (Text -> IO Statement)
    -> EntityDef
    -> IO (Either [Text] [(Bool, Text)])
mockMigrate allDefs _ entity =
    fmap (fmap $ map showAlterDb) $
        return $
            Right $
                mockMigrateStructured allDefs entity

-- | Mock a migration even when the database is not present.
-- This function performs the same functionality of 'printMigration'
-- with the difference that an actual database is not needed.
mockMigration :: Migration -> IO ()
mockMigration mig = do
    smap <- newIORef mempty
    let
        sqlbackend =
            mkSqlBackend
                MkSqlBackendArgs
                    { connPrepare = \_ -> do
                        return
                            Statement
                                { stmtFinalize = return ()
                                , stmtReset = return ()
                                , stmtExecute = undefined
                                , stmtQuery = \_ -> return $ return ()
                                }
                    , connInsertSql = undefined
                    , connStmtMap = smap
                    , connClose = undefined
                    , connMigrateSql = mockMigrate
                    , connBegin = undefined
                    , connCommit = undefined
                    , connRollback = undefined
                    , connEscapeFieldName = escapeF
                    , connEscapeTableName = escapeE . getEntityDBName
                    , connEscapeRawName = escape
                    , connNoLimit = undefined
                    , connRDBMS = undefined
                    , connLimitOffset = undefined
                    , connLogFunc = undefined
                    }
        result = runReaderT $ runWriterT $ runWriterT mig
    resp <- result sqlbackend
    mapM_ T.putStrLn $ map snd $ snd resp

putManySql :: EntityDef -> Int -> Text
putManySql ent n = putManySql' conflictColumns fields ent n
  where
    fields = getEntityFields ent
    conflictColumns =
        concatMap
            (map (escapeF . snd) . NEL.toList . uniqueFields)
            (getEntityUniques ent)

repsertManySql :: EntityDef -> Int -> Text
repsertManySql ent n = putManySql' conflictColumns fields ent n
  where
    fields = NEL.toList $ keyAndEntityFields ent
    conflictColumns = NEL.toList $ escapeF . fieldDB <$> getEntityKeyFields ent

-- | This type is used to determine how to update rows using Postgres'
-- @INSERT ... ON CONFLICT KEY UPDATE@ functionality, exposed via
-- 'upsertWhere' and 'upsertManyWhere' in this library.
--
-- @since 2.12.1.0
data HandleUpdateCollision record where
    -- | Copy the field directly from the record.
    CopyField :: EntityField record typ -> HandleUpdateCollision record
    -- | Only copy the field if it is not equal to the provided value.
    CopyUnlessEq
        :: (PersistField typ)
        => EntityField record typ
        -> typ
        -> HandleUpdateCollision record

-- | Copy the field into the database only if the value in the
-- corresponding record is non-@NULL@.
--
-- @since  2.12.1.0
copyUnlessNull
    :: (PersistField typ)
    => EntityField record (Maybe typ)
    -> HandleUpdateCollision record
copyUnlessNull field = CopyUnlessEq field Nothing

-- | Copy the field into the database only if the value in the
-- corresponding record is non-empty, where "empty" means the Monoid
-- definition for 'mempty'. Useful for 'Text', 'String', 'ByteString', etc.
--
-- The resulting 'HandleUpdateCollision' type is useful for the
-- 'upsertManyWhere' function.
--
-- @since  2.12.1.0
copyUnlessEmpty
    :: (Monoid.Monoid typ, PersistField typ)
    => EntityField record typ
    -> HandleUpdateCollision record
copyUnlessEmpty field = CopyUnlessEq field Monoid.mempty

-- | Copy the field into the database only if the field is not equal to the
-- provided value. This is useful to avoid copying weird nullary data into
-- the database.
--
-- The resulting 'HandleUpdateCollision' type is useful for the
-- 'upsertMany' function.
--
-- @since  2.12.1.0
copyUnlessEq
    :: (PersistField typ)
    => EntityField record typ
    -> typ
    -> HandleUpdateCollision record
copyUnlessEq = CopyUnlessEq

-- | Copy the field directly from the record.
--
-- @since 2.12.1.0
copyField
    :: (PersistField typ) => EntityField record typ -> HandleUpdateCollision record
copyField = CopyField

-- | Postgres specific 'upsertWhere'. This method does the following:
-- It will insert a record if no matching unique key exists.
-- If a unique key exists, it will update the relevant field with a user-supplied value, however,
-- it will only do this update on a user-supplied condition.
-- For example, here's how this method could be called like such:
--
-- @
-- upsertWhere record [recordField =. newValue] [recordField /= newValue]
-- @
--
-- Called thusly, this method will insert a new record (if none exists) OR update a recordField with a new value
-- assuming the condition in the last block is met.
--
-- @since 2.12.1.0
upsertWhere
    :: ( backend ~ PersistEntityBackend record
       , PersistEntity record
       , PersistEntityBackend record ~ SqlBackend
       , MonadIO m
       , PersistStore backend
       , BackendCompatible SqlBackend backend
       , OnlyOneUniqueKey record
       )
    => record
    -> [Update record]
    -> [Filter record]
    -> ReaderT backend m ()
upsertWhere record updates filts =
    upsertManyWhere [record] [] updates filts

-- | Postgres specific 'upsertManyWhere'.
--
-- The first argument is a list of new records to insert.
--
-- If there's unique key collisions for some or all of the proposed insertions,
-- you can use the second argument to specify which fields, under which
-- conditions (using 'HandleUpdateCollision' strategies,) will be copied from
-- each proposed, conflicting new record onto its corresponding row already present
-- in the database. Helpers such as 'copyField', 'copyUnlessEq',
-- 'copyUnlessNull' and 'copyUnlessEmpty' are exported from this module to help
-- with this, as well as the constructors for 'HandleUpdateCollision'.
-- For example, you may have a patch of updates to apply, some of which are NULL
-- to represent "no change"; you'd use 'copyUnlessNull' for each applicable
-- field in this case so any new values in the patch which are NULL don't
-- accidentally overwrite existing values in the database which are non-NULL.
--
-- Further updates to the matching records already in the database can be
-- specified in the third argument, these are arbitrary updates independent of
-- the new records proposed for insertion. For example, you can use this to
-- update a timestamp or a counter for all conflicting rows.
--
-- You can use the fourth argument to provide arbitrary conditions to constrain
-- the above updates. This way, you can choose to only alter a subset of the
-- conflicting existing rows while leaving the others alone. For example, you
-- may want to copy fields from new records and update a timestamp only when the
-- corresponding existing row hasn't been soft-deleted.
--
--
-- For example,
--
-- @
--   upsertManyWhere
--     [record]                         -- (1)
--     [ copyField recordField1         -- (2)
--     , copyUnlessEq recordField2      -- (3)
--     , copyUnlessNull recordField3    -- (4)
--     ]
--     [recordField4 =. arbitraryValue] -- (5)
--     [recordField4 !=. anotherValue]  -- (6)
-- @
--
-- 1. new records to insert if there's no conflicts
-- 2. for each conflicting existing row, replace the value of recordField1 with the one present in the conflicting new record
-- 3. only replace the existing value if it's different from the one present in the conflicting new record
-- 4. only replace the existing value if the new value is non-NULL (i.e. don't replace existing values with NULLs.)
--
-- 5. update recordField4 with an arbitrary new value
-- 6. only apply the above updates for conflicting rows that meet this condition
--
-- Called thusly, this method will insert a new record (if none exists) OR it
-- will copy three fields (@recordField1@, @recordField2@, @recordField3@) from
-- the record proposed from insertion onto the existing row in the database
-- following three different 'HandleUpdateCollision' strategies, it will /also/
-- update @recordField4@ with an arbitrary @arbitraryValue@. Both sets of updates
-- will only be applied if the row already present in the database has a
-- @recordField4@ whose value is not @arbitraryValue@. The corresponding SQL might
-- look like this:
--
-- @
--   INSERT INTO table(recordField1, recordField2, recordField3, recordField4, recordField5)
--   VALUES
--     (newValue1, newValue2, newValue3, newValue4, newValue5)
--   ON CONFLICT (recordField2, recordField3) -- let's imagine these two columns define a unique constraint
--   DO UPDATE
--     SET
--       recordField1 = EXCLUDED.newValue1, -- EXCLUDED points to the new, conflicting, record; so we're always replacing the old value.
--       recordField2 = COALESCE(NULLIF(EXCLUDED.newValue2, table.recordField2), table.recordField2), -- if the values are the same, the NULLIF returns NULL and we coalesce into setting the column to the value it already had (i.e. no change)
--       recordField3 = COALESCE(NULLIF(EXCLUDED.newValue3, NULL), table.recordField3), -- if the new value is not null, it will be set; if it's null, we coalesce into not changing the existing value.
--       recordField4 = arbitraryValue -- an arbitrary update independent of what's in the EXCLUDED (new) record
--     WHERE
--       recordField4 <> anotherValue -- only do the above updates if the value for the existing row's recordField4 is not "anotherValue"
-- @
--
-- @since 2.12.1.0
upsertManyWhere
    :: forall record backend m
     . ( backend ~ PersistEntityBackend record
       , BackendCompatible SqlBackend backend
       , PersistEntityBackend record ~ SqlBackend
       , PersistEntity record
       , OnlyOneUniqueKey record
       , MonadIO m
       )
    => [record]
    -- ^ A list of the records you want to insert, or update
    -> [HandleUpdateCollision record]
    -- ^ A list of the fields you want to copy over.
    -> [Update record]
    -- ^ A list of the updates to apply that aren't dependent on the record
    -- being inserted.
    -> [Filter record]
    -- ^ A filter condition that dictates the scope of the updates
    -> ReaderT backend m ()
upsertManyWhere [] _ _ _ = return ()
upsertManyWhere records fieldValues updates filters = do
    conn <- asks projectBackend
    let
        uniqDef = onlyOneUniqueDef (Proxy :: Proxy record)
    uncurry rawExecute $
        mkBulkUpsertQuery records conn fieldValues updates filters uniqDef

-- | Exclude any record field if it doesn't match the filter record.  Used only in `upsertWhere` and
-- `upsertManyWhere`
--
-- TODO: we could probably make a sum type for the `Filter` record that's passed into the `upsertWhere` and
-- `upsertManyWhere` methods that has similar behavior to the HandleCollisionUpdate type.
--
-- @since 2.12.1.0
excludeNotEqualToOriginal
    :: (PersistField typ, PersistEntity rec)
    => EntityField rec typ
    -> Filter rec
excludeNotEqualToOriginal field =
    Filter
        { filterField =
            field
        , filterFilter =
            Ne
        , filterValue =
            UnsafeValue $
                PersistLiteral_
                    Unescaped
                    bsForExcludedField
        }
  where
    bsForExcludedField =
        T.encodeUtf8 $
            "EXCLUDED."
                <> fieldName field

-- | This creates the query for 'upsertManyWhere'. If you
-- provide an empty list of updates to perform, then it will generate
-- a dummy/no-op update using the first field of the record. This avoids
-- duplicate key exceptions.
mkBulkUpsertQuery
    :: ( PersistEntity record
       , PersistEntityBackend record ~ SqlBackend
       , OnlyOneUniqueKey record
       )
    => [record]
    -- ^ A list of the records you want to insert, or update
    -> SqlBackend
    -> [HandleUpdateCollision record]
    -- ^ A list of the fields you want to copy over.
    -> [Update record]
    -- ^ A list of the updates to apply that aren't dependent on the record being inserted.
    -> [Filter record]
    -- ^ A filter condition that dictates the scope of the updates
    -> UniqueDef
    -- ^ The specific uniqueness constraint to use on the record. Postgres
    -- rquires that we use exactly one relevant constraint, and it can't do
    -- a catch-all. How frustrating!
    -> (Text, [PersistValue])
mkBulkUpsertQuery records conn fieldValues updates filters uniqDef =
    (q, recordValues <> updsValues <> copyUnlessValues <> whereVals)
  where
    mfieldDef x = case x of
        CopyField rec -> Right (fieldDbToText (persistFieldDef rec))
        CopyUnlessEq rec val -> Left (fieldDbToText (persistFieldDef rec), toPersistValue val)
    (fieldsToMaybeCopy, updateFieldNames) = partitionEithers $ map mfieldDef fieldValues
    fieldDbToText = escapeF . fieldDB
    entityDef' = entityDef records
    conflictColumns =
        map (escapeF . snd) $ NEL.toList $ uniqueFields uniqDef
    firstField = case entityFieldNames of
        [] -> error "The entity you're trying to insert does not have any fields."
        (field : _) -> field
    entityFieldNames = map fieldDbToText (getEntityFields entityDef')
    nameOfTable = escapeE . getEntityDBName $ entityDef'
    copyUnlessValues = map snd fieldsToMaybeCopy
    recordValues = concatMap (map toPersistValue . toPersistFields) records
    recordPlaceholders =
        Util.commaSeparated
            $ map
                (Util.parenWrapped . Util.commaSeparated . map (const "?") . toPersistFields)
            $ records
    mkCondFieldSet n _ =
        T.concat
            [ n
            , "=COALESCE("
            , "NULLIF("
            , "EXCLUDED."
            , n
            , ","
            , "?"
            , ")"
            , ","
            , nameOfTable
            , "."
            , n
            , ")"
            ]
    condFieldSets = map (uncurry mkCondFieldSet) fieldsToMaybeCopy
    fieldSets = map (\n -> T.concat [n, "=EXCLUDED.", n, ""]) updateFieldNames
    upds =
        map
            (Util.mkUpdateText' (escapeF) (\n -> T.concat [nameOfTable, ".", n]))
            updates
    updsValues = map (\(Update _ val _) -> toPersistValue val) updates
    (wher, whereVals) =
        if null filters
            then ("", [])
            else (filterClauseWithVals (Just PrefixTableName) conn filters)
    updateText =
        case fieldSets <> upds <> condFieldSets of
            [] ->
                -- This case is really annoying, and probably unlikely to be
                -- actually hit - someone would have had to call something like
                -- `upsertManyWhere [] [] []`, but that would have been caught
                -- by the prior case.
                -- Would be nice to have something like a `NonEmpty (These ...)`
                -- instead of multiple lists...
                T.concat [firstField, "=", nameOfTable, ".", firstField]
            xs ->
                Util.commaSeparated xs
    q =
        T.concat
            [ "INSERT INTO "
            , nameOfTable
            , Util.parenWrapped . Util.commaSeparated $ entityFieldNames
            , " VALUES "
            , recordPlaceholders
            , " ON CONFLICT "
            , Util.parenWrapped $ Util.commaSeparated $ conflictColumns
            , " DO UPDATE SET "
            , updateText
            , wher
            ]

putManySql' :: [Text] -> [FieldDef] -> EntityDef -> Int -> Text
putManySql' conflictColumns (filter isFieldNotGenerated -> fields) ent n = q
  where
    fieldDbToText = escapeF . fieldDB
    mkAssignment f = T.concat [f, "=EXCLUDED.", f]

    table = escapeE . getEntityDBName $ ent
    columns = Util.commaSeparated $ map fieldDbToText fields
    placeholders = map (const "?") fields
    updates = map (mkAssignment . fieldDbToText) fields

    q =
        T.concat
            [ "INSERT INTO "
            , table
            , Util.parenWrapped columns
            , " VALUES "
            , Util.commaSeparated
                . replicate n
                . Util.parenWrapped
                . Util.commaSeparated
                $ placeholders
            , " ON CONFLICT "
            , Util.parenWrapped . Util.commaSeparated $ conflictColumns
            , " DO UPDATE SET "
            , Util.commaSeparated updates
            ]

-- | Enable a Postgres extension. See https://www.postgresql.org/docs/current/static/contrib.html
-- for a list.
migrateEnableExtension :: Text -> Migration
migrateEnableExtension extName = WriterT $ WriterT $ do
    res :: [Single Int] <-
        rawSql
            "SELECT COUNT(*) FROM pg_catalog.pg_extension WHERE extname = ?"
            [PersistText extName]
    if res == [Single 0]
        then return (((), []), [(False, "CREATe EXTENSION \"" <> extName <> "\"")])
        else return (((), []), [])

-- | Wrapper for persistent SqlBackends that carry the corresponding
-- `Postgresql.Connection`.
--
-- @since 2.13.1.0
data RawPostgresql backend = RawPostgresql
    { persistentBackend :: backend
    -- ^ The persistent backend
    --
    -- @since 2.13.1.0
    , rawPostgresqlConnection :: PG.Connection
    -- ^ The underlying `PG.Connection`
    --
    -- @since 2.13.1.0
    }

instance BackendCompatible (RawPostgresql b) (RawPostgresql b) where
    projectBackend = id

instance BackendCompatible b (RawPostgresql b) where
    projectBackend = persistentBackend

withRawConnection
    :: (PG.Connection -> SqlBackend)
    -> PG.Connection
    -> RawPostgresql SqlBackend
withRawConnection f conn =
    RawPostgresql
        { persistentBackend = f conn
        , rawPostgresqlConnection = conn
        }

-- | Create a PostgreSQL connection pool which also exposes the
-- raw connection. The raw counterpart to 'createPostgresqlPool'.
--
-- @since 2.13.1.0
createRawPostgresqlPool
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open
    -- in the pool.
    -> m (Pool (RawPostgresql SqlBackend))
createRawPostgresqlPool = createRawPostgresqlPoolModified (const $ return ())

-- | The raw counterpart to 'createPostgresqlPoolModified'.
--
-- @since 2.13.1.0
createRawPostgresqlPoolModified
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO ())
    -- ^ Action to perform after connection is created.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in the pool.
    -> m (Pool (RawPostgresql SqlBackend))
createRawPostgresqlPoolModified = createRawPostgresqlPoolModifiedWithVersion getServerVersion

-- | The raw counterpart to 'createPostgresqlPoolModifiedWithVersion'.
--
-- @since 2.13.1.0
createRawPostgresqlPoolModifiedWithVersion
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO (Maybe Double))
    -- ^ Action to perform to get the server version.
    -> (PG.Connection -> IO ())
    -- ^ Action to perform after connection is created.
    -> ConnectionString
    -- ^ Connection string to the database.
    -> Int
    -- ^ Number of connections to be kept open in the pool.
    -> m (Pool (RawPostgresql SqlBackend))
createRawPostgresqlPoolModifiedWithVersion getVerDouble modConn ci = do
    let
        getVer = oldGetVersionToNew getVerDouble
    createSqlPool $ open' modConn getVer withRawConnection ci

-- | The raw counterpart to 'createPostgresqlPoolWithConf'.
--
-- @since 2.13.1.0
createRawPostgresqlPoolWithConf
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => PostgresConf
    -- ^ Configuration for connecting to Postgres
    -> PostgresConfHooks
    -- ^ Record of callback functions
    -> m (Pool (RawPostgresql SqlBackend))
createRawPostgresqlPoolWithConf conf hooks = do
    let
        getVer = pgConfHooksGetServerVersion hooks
        modConn = pgConfHooksAfterCreate hooks
    createSqlPoolWithConfig
        (open' modConn getVer withRawConnection (pgConnStr conf))
        (postgresConfToConnectionPoolConfig conf)

#if MIN_VERSION_base(4,12,0)
instance (PersistCore b) => PersistCore (RawPostgresql b) where
  newtype BackendKey (RawPostgresql b) = RawPostgresqlKey { unRawPostgresqlKey :: BackendKey (Compatible b (RawPostgresql b)) }

makeCompatibleKeyInstances [t| forall b. Compatible b (RawPostgresql b) |]
#else
instance (PersistCore b) => PersistCore (RawPostgresql b) where
  newtype BackendKey (RawPostgresql b) = RawPostgresqlKey { unRawPostgresqlKey :: BackendKey (RawPostgresql b) }

deriving instance (Show (BackendKey b)) => Show (BackendKey (RawPostgresql b))
deriving instance (Read (BackendKey b)) => Read (BackendKey (RawPostgresql b))
deriving instance (Eq (BackendKey b)) => Eq (BackendKey (RawPostgresql b))
deriving instance (Ord (BackendKey b)) => Ord (BackendKey (RawPostgresql b))
deriving instance (Num (BackendKey b)) => Num (BackendKey (RawPostgresql b))
deriving instance (Integral (BackendKey b)) => Integral (BackendKey (RawPostgresql b))
deriving instance (PersistField (BackendKey b)) => PersistField (BackendKey (RawPostgresql b))
deriving instance (PersistFieldSql (BackendKey b)) => PersistFieldSql (BackendKey (RawPostgresql b))
deriving instance (Real (BackendKey b)) => Real (BackendKey (RawPostgresql b))
deriving instance (Enum (BackendKey b)) => Enum (BackendKey (RawPostgresql b))
deriving instance (Bounded (BackendKey b)) => Bounded (BackendKey (RawPostgresql b))
deriving instance (ToJSON (BackendKey b)) => ToJSON (BackendKey (RawPostgresql b))
deriving instance (FromJSON (BackendKey b)) => FromJSON (BackendKey (RawPostgresql b))
#endif

#if MIN_VERSION_base(4,12,0)
$(pure [])

makeCompatibleInstances [t| forall b. Compatible b (RawPostgresql b) |]
#else
instance HasPersistBackend b => HasPersistBackend (RawPostgresql b) where
    type BaseBackend (RawPostgresql b) = BaseBackend b
    persistBackend = persistBackend . persistentBackend

instance (PersistStoreRead b) => PersistStoreRead (RawPostgresql b) where
    get = withReaderT persistentBackend . get
    getMany = withReaderT persistentBackend . getMany

instance (PersistQueryRead b) => PersistQueryRead (RawPostgresql b) where
    selectSourceRes filts opts = withReaderT persistentBackend $ selectSourceRes filts opts
    selectFirst filts opts = withReaderT persistentBackend $ selectFirst filts opts
    selectKeysRes filts opts = withReaderT persistentBackend $ selectKeysRes filts opts
    count = withReaderT persistentBackend . count
    exists = withReaderT persistentBackend . exists

instance (PersistQueryWrite b) => PersistQueryWrite (RawPostgresql b) where
    updateWhere filts updates = withReaderT persistentBackend $ updateWhere filts updates
    deleteWhere = withReaderT persistentBackend . deleteWhere

instance (PersistUniqueRead b) => PersistUniqueRead (RawPostgresql b) where
    getBy = withReaderT persistentBackend . getBy

instance (PersistStoreWrite b) => PersistStoreWrite (RawPostgresql b) where
    insert = withReaderT persistentBackend . insert
    insert_ = withReaderT persistentBackend . insert_
    insertMany = withReaderT persistentBackend . insertMany
    insertMany_ = withReaderT persistentBackend . insertMany_
    insertEntityMany = withReaderT persistentBackend . insertEntityMany
    insertKey k = withReaderT persistentBackend . insertKey k
    repsert k = withReaderT persistentBackend . repsert k
    repsertMany = withReaderT persistentBackend . repsertMany
    replace k = withReaderT persistentBackend . replace k
    delete = withReaderT persistentBackend . delete
    update k = withReaderT persistentBackend . update k
    updateGet k = withReaderT persistentBackend . updateGet k

instance (PersistUniqueWrite b) => PersistUniqueWrite (RawPostgresql b) where
    deleteBy = withReaderT persistentBackend . deleteBy
    insertUnique = withReaderT persistentBackend . insertUnique
    upsert rec = withReaderT persistentBackend . upsert rec
    upsertBy uniq rec = withReaderT persistentBackend . upsertBy uniq rec
    putMany = withReaderT persistentBackend . putMany
#endif
