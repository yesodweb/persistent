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
    , withPostgresqlConn
    , withPostgresqlConnWithVersion
    , withPostgresqlPoolWithConf
    , createPostgresqlPool
    , createPostgresqlPoolModified
    , createPostgresqlPoolModifiedWithVersion
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
    , tableName
    , fieldName
    , mockMigration
    , migrateEnableExtension
    , PostgresConfHooks(..)
    , defaultPostgresConfHooks

    , RawPostgresql(..)
    , createRawPostgresqlPool
    , createRawPostgresqlPoolModified
    , createRawPostgresqlPoolModifiedWithVersion
    , createRawPostgresqlPoolWithConf
    ) where

import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Internal as PG
import Database.PostgreSQL.Simple.Ok (Ok(..))
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Database.PostgreSQL.Simple.Types as PG

import Control.Arrow
import Control.Exception (Exception, throw, throwIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift (MonadIO(..), MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT(..), asks, runReaderT)
#if !MIN_VERSION_base(4,12,0)
import Control.Monad.Trans.Reader (withReaderT)
#endif
import Control.Monad.Trans.Writer (WriterT(..), runWriterT)
import qualified Data.List.NonEmpty as NEL
import Data.Proxy (Proxy(..))

import Data.Acquire (Acquire, mkAcquire, with)
import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import qualified Data.Attoparsec.Text as AT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.IORef
import Data.Int (Int64)
import Data.List (find, foldl', groupBy, sort)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
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
import Database.Persist.Postgresql.Internal
import Database.Persist.Sql
import qualified Database.Persist.Sql.Util as Util
import Database.Persist.SqlBackend

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
withPostgresqlPool :: (MonadLoggerIO m, MonadUnliftIO m)
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
withPostgresqlPoolWithVersion :: (MonadUnliftIO m, MonadLoggerIO m)
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
  let getVer = oldGetVersionToNew getVerDouble
  withSqlPool $ open' (const $ return ()) getVer id ci

-- | Same as 'withPostgresqlPool', but can be configured with 'PostgresConf' and 'PostgresConfHooks'.
--
-- @since 2.11.0.0
withPostgresqlPoolWithConf :: (MonadUnliftIO m, MonadLoggerIO m)
                           => PostgresConf -- ^ Configuration for connecting to Postgres
                           -> PostgresConfHooks -- ^ Record of callback functions
                           -> (Pool SqlBackend -> m a)
                           -- ^ Action to be executed that uses the
                           -- connection pool.
                           -> m a
withPostgresqlPoolWithConf conf hooks = do
  let getVer = pgConfHooksGetServerVersion hooks
      modConn = pgConfHooksAfterCreate hooks
  let logFuncToBackend = open' modConn getVer id (pgConnStr conf)
  withSqlPoolWithConfig logFuncToBackend (postgresConfToConnectionPoolConfig conf)

-- | Create a PostgreSQL connection pool.  Note that it's your
-- responsibility to properly close the connection pool when
-- unneeded.  Use 'withPostgresqlPool' for an automatic resource
-- control.
createPostgresqlPool :: (MonadUnliftIO m, MonadLoggerIO m)
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
    => (PG.Connection -> IO ()) -- ^ Action to perform after connection is created.
    -> ConnectionString -- ^ Connection string to the database.
    -> Int -- ^ Number of connections to be kept open in the pool.
    -> m (Pool SqlBackend)
createPostgresqlPoolModified = createPostgresqlPoolModifiedWithVersion getServerVersion

-- | Same as other similarly-named functions in this module, but takes callbacks for obtaining
-- the server version (to work around an Amazon Redshift bug) and connection-specific tweaking
-- (to change the schema).
--
-- @since 2.6.2
createPostgresqlPoolModifiedWithVersion
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO (Maybe Double)) -- ^ Action to perform to get the server version.
    -> (PG.Connection -> IO ()) -- ^ Action to perform after connection is created.
    -> ConnectionString -- ^ Connection string to the database.
    -> Int -- ^ Number of connections to be kept open in the pool.
    -> m (Pool SqlBackend)
createPostgresqlPoolModifiedWithVersion getVerDouble modConn ci = do
  let getVer = oldGetVersionToNew getVerDouble
  createSqlPool $ open' modConn getVer id ci

-- | Same as 'createPostgresqlPool', but can be configured with 'PostgresConf' and 'PostgresConfHooks'.
--
-- @since 2.11.0.0
createPostgresqlPoolWithConf
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => PostgresConf -- ^ Configuration for connecting to Postgres
    -> PostgresConfHooks -- ^ Record of callback functions
    -> m (Pool SqlBackend)
createPostgresqlPoolWithConf conf hooks = do
  let getVer = pgConfHooksGetServerVersion hooks
      modConn = pgConfHooksAfterCreate hooks
  createSqlPoolWithConfig (open' modConn getVer id (pgConnStr conf)) (postgresConfToConnectionPoolConfig conf)

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
withPostgresqlConn :: (MonadUnliftIO m, MonadLoggerIO m)
                   => ConnectionString -> (SqlBackend -> m a) -> m a
withPostgresqlConn = withPostgresqlConnWithVersion getServerVersion

-- | Same as 'withPostgresqlConn', but takes a callback for obtaining
-- the server version (to work around an Amazon Redshift bug).
--
-- @since 2.6.2
withPostgresqlConnWithVersion :: (MonadUnliftIO m, MonadLoggerIO m)
                              => (PG.Connection -> IO (Maybe Double))
                              -> ConnectionString
                              -> (SqlBackend -> m a)
                              -> m a
withPostgresqlConnWithVersion getVerDouble = do
  let getVer = oldGetVersionToNew getVerDouble
  withSqlConn . open' (const $ return ()) getVer id

open'
    :: (PG.Connection -> IO ())
    -> (PG.Connection -> IO (NonEmpty Word))
    -> ((PG.Connection -> SqlBackend) -> PG.Connection -> backend)
    -- ^ How to construct the actual backend type desired. For most uses,
    -- this is just 'id', since the desired backend type is 'SqlBackend'.
    -- But some callers want a @'RawPostgresql' 'SqlBackend'@, and will
    -- pass in 'withRawConnection'.
    -> ConnectionString -> LogFunc -> IO backend
open' modConn getVer constructor cstr logFunc = do
    conn <- PG.connectPostgreSQL cstr
    modConn conn
    ver <- getVer conn
    smap <- newIORef $ Map.empty
    return $ constructor (createBackend logFunc ver smap) conn

-- | Gets the PostgreSQL server version
getServerVersion :: PG.Connection -> IO (Maybe Double)
getServerVersion conn = do
  [PG.Only version] <- PG.query_ conn "show server_version";
  let version' = rational version
  --- λ> rational "9.8.3"
  --- Right (9.8,".3")
  --- λ> rational "9.8.3.5"
  --- Right (9.8,".3.5")
  case version' of
    Right (a,_) -> return $ Just a
    Left err -> throwIO $ PostgresServerVersionError err

getServerVersionNonEmpty :: PG.Connection -> IO (NonEmpty Word)
getServerVersionNonEmpty conn = do
  [PG.Only version] <- PG.query_ conn "show server_version";
  case AT.parseOnly parseVersion (T.pack version) of
    Left err -> throwIO $ PostgresServerVersionError $ "Parse failure on: " <> version <> ". Error: " <> err
    Right versionComponents -> case NEL.nonEmpty versionComponents of
      Nothing -> throwIO $ PostgresServerVersionError $ "Empty Postgres version string: " <> version
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
upsertFunction f version = if (version >= postgres9dot5)
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

oldGetVersionToNew :: (PG.Connection -> IO (Maybe Double)) -> (PG.Connection -> IO (NonEmpty Word))
oldGetVersionToNew oldFn = \conn -> do
  mDouble <- oldFn conn
  case mDouble of
    Nothing -> pure minimumPostgresVersion
    Just double -> do
      let (major, minor) = properFraction double
      pure $ major NEL.:| [floor minor]

-- | Generate a 'SqlBackend' from a 'PG.Connection'.
openSimpleConn :: LogFunc -> PG.Connection -> IO SqlBackend
openSimpleConn = openSimpleConnWithVersion getServerVersion

-- | Generate a 'SqlBackend' from a 'PG.Connection', but takes a callback for
-- obtaining the server version.
--
-- @since 2.9.1
openSimpleConnWithVersion :: (PG.Connection -> IO (Maybe Double)) -> LogFunc -> PG.Connection -> IO SqlBackend
openSimpleConnWithVersion getVerDouble logFunc conn = do
    smap <- newIORef $ Map.empty
    serverVersion <- oldGetVersionToNew getVerDouble conn
    return $ createBackend logFunc serverVersion smap conn

-- | Create the backend given a logging function, server version, mutable statement cell,
-- and connection.
createBackend :: LogFunc -> NonEmpty Word
              -> IORef (Map.Map Text Statement) -> PG.Connection -> SqlBackend
createBackend logFunc serverVersion smap conn =
    maybe id setConnPutManySql (upsertFunction putManySql serverVersion) $
    maybe id setConnUpsertSql (upsertFunction upsertSql' serverVersion) $
    setConnInsertManySql insertManySql' $
    maybe id setConnRepsertManySql (upsertFunction repsertManySql serverVersion) $
    mkSqlBackend MkSqlBackendArgs
        { connPrepare    = prepare' conn
        , connStmtMap    = smap
        , connInsertSql  = insertSql'
        , connClose      = PG.close conn
        , connMigrateSql = migrate'
        , connBegin      = \_ mIsolation -> case mIsolation of
              Nothing -> PG.begin conn
              Just iso -> PG.beginLevel (case iso of
                  ReadUncommitted -> PG.ReadCommitted -- PG Upgrades uncommitted reads to committed anyways
                  ReadCommitted -> PG.ReadCommitted
                  RepeatableRead -> PG.RepeatableRead
                  Serializable -> PG.Serializable) conn
        , connCommit     = const $ PG.commit   conn
        , connRollback   = const $ PG.rollback conn
        , connEscapeFieldName = escapeF
        , connEscapeTableName = escapeE . getEntityDBName
        , connEscapeRawName = escape
        , connNoLimit    = "LIMIT ALL"
        , connRDBMS      = "postgresql"
        , connLimitOffset = decorateSQLWithLimitOffset "LIMIT ALL"
        , connLogFunc = logFunc
        }

prepare' :: PG.Connection -> Text -> IO Statement
prepare' conn sql = do
    let query = PG.Query (T.encodeUtf8 sql)
    return Statement
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
    sql = T.concat
        [ "INSERT INTO "
        , escapeE $ getEntityDBName ent
        , if null (getEntityFields ent)
            then " DEFAULT VALUES"
            else T.concat
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
    (fieldNames, placeholders)= unzip (Util.mkInsertPlaceholders ent escapeF)
    sql = T.concat
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

withStmt' :: MonadIO m
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
                         Just e  -> "Postgresql.withStmt': " ++ B8.unpack e
              Just ret -> do
                -- Check result status
                status <- LibPQ.resultStatus ret
                case status of
                  LibPQ.TuplesOk -> return ()
                  _ -> PG.throwResultError "Postgresql.withStmt': bad result status " ret status

                -- Get number and type of columns
                cols <- LibPQ.nfields ret
                oids <- forM [0..cols-1] $ \col -> fmap ((,) col) (LibPQ.ftype ret col)
                -- Ready to go!
                rowRef   <- newIORef (LibPQ.Row 0)
                rowCount <- LibPQ.ntuples ret
                return (ret, rowRef, rowCount, oids)
      let getters
            = map (\(col, oid) -> getGetter oid $ PG.Field rt col oid) ids
      return (rt, rr, rc, getters)

    closeS (ret, _, _, _) = LibPQ.unsafeFreeResult ret

    pull x = do
        y <- liftIO $ pullS x
        case y of
            Nothing -> return ()
            Just z -> yield z >> pull x

    pullS (ret, rowRef, rowCount, getters) = do
        row <- atomicModifyIORef rowRef (\r -> (r+1, r))
        if row == rowCount
           then return Nothing
           else fmap Just $ forM (zip getters [0..]) $ \(getter, col) -> do
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
                                                        Errors (exc:_) -> throw exc
                                                        Errors [] -> error "Got an Errors, but no exceptions"
                                                        Ok v  -> return v

doesTableExist :: (Text -> IO Statement)
               -> EntityNameDB
               -> IO Bool
doesTableExist getter (EntityNameDB name) = do
    stmt <- getter sql
    with (stmtQuery stmt vals) (\src -> runConduit $ src .| start)
  where
    sql = "SELECT COUNT(*) FROM pg_catalog.pg_tables WHERE schemaname != 'pg_catalog'"
          <> " AND schemaname != 'information_schema' AND tablename=?"
    vals = [PersistText name]

    start = await >>= maybe (error "No results when checking doesTableExist") start'
    start' [PersistInt64 0] = finish False
    start' [PersistInt64 1] = finish True
    start' res = error $ "doesTableExist returned unexpected result: " ++ show res
    finish x = await >>= maybe (return x) (error "Too many rows returned in doesTableExist")

migrate' :: [EntityDef]
         -> (Text -> IO Statement)
         -> EntityDef
         -> IO (Either [Text] [(Bool, Text)])
migrate' allDefs getter entity = fmap (fmap $ map showAlterDb) $ do
    old <- getColumns getter entity newcols'
    case partitionEithers old of
        ([], old'') -> do
            exists' <-
                if null old
                    then doesTableExist getter name
                    else return True
            return $ Right $ migrationText exists' old''
        (errs, _) -> return $ Left errs
  where
    name = getEntityDBName entity
    (newcols', udefs, fdefs) = postgresMkColumns allDefs entity
    migrationText exists' old''
        | not exists' =
            createText newcols fdefs udspair
        | otherwise =
            let (acs, ats) =
                    getAlters allDefs entity (newcols, udspair) old'
                acs' = map (AlterColumn name) acs
                ats' = map (AlterTable name) ats
            in
                acs' ++ ats'
       where
         old' = partitionEithers old''
         newcols = filter (not . safeToRemove entity . cName) newcols'
         udspair = map udToPair udefs
            -- Check for table existence if there are no columns, workaround
            -- for https://github.com/yesodweb/persistent/issues/152

    createText newcols fdefs_ udspair =
        (addTable newcols entity) : uniques ++ references ++ foreignsAlt
      where
        uniques = flip concatMap udspair $ \(uname, ucols) ->
                [AlterTable name $ AddUniqueConstraint uname ucols]
        references =
            mapMaybe
                (\Column { cName, cReference } ->
                    getAddReference allDefs entity cName =<< cReference
                )
                newcols
        foreignsAlt = mapMaybe (mkForeignAlt entity) fdefs_

mkForeignAlt
    :: EntityDef
    -> ForeignDef
    -> Maybe AlterDB
mkForeignAlt entity fdef = pure $ AlterColumn tableName_ addReference
  where
    tableName_ = getEntityDBName entity
    addReference =
        AddReference
            (foreignRefTableDBName fdef)
            constraintName
            childfields
            escapedParentFields
            (foreignFieldCascade fdef)
    constraintName =
        foreignConstraintNameDBName fdef
    (childfields, parentfields) =
        unzip (map (\((_,b),(_,d)) -> (b,d)) (foreignFields fdef))
    escapedParentFields =
        map escapeF parentfields

addTable :: [Column] -> EntityDef -> AlterDB
addTable cols entity =
    AddTable $ T.concat
        -- Lower case e: see Database.Persist.Sql.Migration
        [ "CREATe TABLE " -- DO NOT FIX THE CAPITALIZATION!
        , escapeE name
        , "("
        , idtxt
        , if null nonIdCols then "" else ","
        , T.intercalate "," $ map showColumn nonIdCols
        , ")"
        ]
  where
    nonIdCols =
        case entityPrimary entity of
            Just _ ->
                cols
            _ ->
                filter keepField cols
      where
        keepField c =
            Just (cName c) /= fmap fieldDB (getEntityIdField entity)
            && not (safeToRemove entity (cName c))

    name =
        getEntityDBName entity
    idtxt =
        case getEntityId entity of
            EntityIdNaturalKey pdef ->
                T.concat
                    [ " PRIMARY KEY ("
                    , T.intercalate "," $ map (escapeF . fieldDB) $ NEL.toList $ compositeFields pdef
                    , ")"
                    ]
            EntityIdField field ->
                let defText = defaultAttribute $ fieldAttrs field
                    sType = fieldSqlType field
                in  T.concat
                        [ escapeF $ fieldDB field
                        , maySerial sType defText
                        , " PRIMARY KEY UNIQUE"
                        , mayDefault defText
                        ]

maySerial :: SqlType -> Maybe Text -> Text
maySerial SqlInt64 Nothing = " SERIAL8 "
maySerial sType _ = " " <> showSqlType sType

mayDefault :: Maybe Text -> Text
mayDefault def = case def of
    Nothing -> ""
    Just d -> " DEFAULT " <> d

type SafeToRemove = Bool

data AlterColumn
    = ChangeType Column SqlType Text
    | IsNull Column
    | NotNull Column
    | Add' Column
    | Drop Column SafeToRemove
    | Default Column Text
    | NoDefault Column
    | Update' Column Text
    | AddReference EntityNameDB ConstraintNameDB [FieldNameDB] [Text] FieldCascade
    | DropReference ConstraintNameDB
    deriving Show

data AlterTable
    = AddUniqueConstraint ConstraintNameDB [FieldNameDB]
    | DropConstraint ConstraintNameDB
    deriving Show

data AlterDB = AddTable Text
             | AlterColumn EntityNameDB AlterColumn
             | AlterTable EntityNameDB AlterTable
             deriving Show

-- | Returns all of the columns in the given table currently in the database.
getColumns :: (Text -> IO Statement)
           -> EntityDef -> [Column]
           -> IO [Either Text (Either Column (ConstraintNameDB, [FieldNameDB]))]
getColumns getter def cols = do
    let sqlv = T.concat
            [ "SELECT "
            , "column_name "
            , ",is_nullable "
            , ",COALESCE(domain_name, udt_name)" -- See DOMAINS below
            , ",column_default "
            , ",generation_expression "
            , ",numeric_precision "
            , ",numeric_scale "
            , ",character_maximum_length "
            , "FROM information_schema.columns "
            , "WHERE table_catalog=current_database() "
            , "AND table_schema=current_schema() "
            , "AND table_name=? "
            ]

-- DOMAINS Postgres supports the concept of domains, which are data types
-- with optional constraints.  An app might make an "email" domain over the
-- varchar type, with a CHECK that the emails are valid In this case the
-- generated SQL should use the domain name: ALTER TABLE users ALTER COLUMN
-- foo TYPE email This code exists to use the domain name (email), instead
-- of the underlying type (varchar).  This is tested in
-- EquivalentTypeTest.hs

    stmt <- getter sqlv
    let vals =
            [ PersistText $ unEntityNameDB $ getEntityDBName def
            ]
    columns <- with (stmtQuery stmt vals) (\src -> runConduit $ src .| processColumns .| CL.consume)
    let sqlc = T.concat
            [ "SELECT "
            , "c.constraint_name, "
            , "c.column_name "
            , "FROM information_schema.key_column_usage AS c, "
            , "information_schema.table_constraints AS k "
            , "WHERE c.table_catalog=current_database() "
            , "AND c.table_catalog=k.table_catalog "
            , "AND c.table_schema=current_schema() "
            , "AND c.table_schema=k.table_schema "
            , "AND c.table_name=? "
            , "AND c.table_name=k.table_name "
            , "AND c.constraint_name=k.constraint_name "
            , "AND NOT k.constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY') "
            , "ORDER BY c.constraint_name, c.column_name"
            ]

    stmt' <- getter sqlc

    us <- with (stmtQuery stmt' vals) (\src -> runConduit $ src .| helperU)
    return $ columns ++ us
  where
    refMap =
        fmap (\cr -> (crTableName cr, crConstraintName cr))
        $ Map.fromList
        $ foldl' ref [] cols
      where
        ref rs c =
            maybe rs (\r -> (unFieldNameDB $ cName c, r) : rs) (cReference c)
    getAll =
        CL.mapM $ \x ->
            pure $ case x of
                [PersistText con, PersistText col] ->
                    (con, col)
                [PersistByteString con, PersistByteString col] ->
                    (T.decodeUtf8 con, T.decodeUtf8 col)
                o ->
                    error $ "unexpected datatype returned for postgres o="++show o
    helperU = do
        rows <- getAll .| CL.consume
        return $ map (Right . Right . (ConstraintNameDB . fst . head &&& map (FieldNameDB . snd)))
               $ groupBy ((==) `on` fst) rows
    processColumns =
        CL.mapM $ \x'@((PersistText cname) : _) -> do
            col <- liftIO $ getColumn getter (getEntityDBName def) x' (Map.lookup cname refMap)
            pure $ case col of
                Left e -> Left e
                Right c -> Right $ Left c

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

getAlters :: [EntityDef]
          -> EntityDef
          -> ([Column], [(ConstraintNameDB, [FieldNameDB])])
          -> ([Column], [(ConstraintNameDB, [FieldNameDB])])
          -> ([AlterColumn], [AlterTable])
getAlters defs def (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old =
        map (\x -> Drop x $ safeToRemove def $ cName x) old
    getAltersC (new:news) old =
        let (alters, old') = findAlters defs def new old
         in alters ++ getAltersC news old'

    getAltersU
        :: [(ConstraintNameDB, [FieldNameDB])]
        -> [(ConstraintNameDB, [FieldNameDB])]
        -> [AlterTable]
    getAltersU [] old =
        map DropConstraint $ filter (not . isManual) $ map fst old
    getAltersU ((name, cols):news) old =
        case lookup name old of
            Nothing ->
                AddUniqueConstraint name cols : getAltersU news old
            Just ocols ->
                let old' = filter (\(x, _) -> x /= name) old
                 in if sort cols == sort ocols
                        then getAltersU news old'
                        else  DropConstraint name
                            : AddUniqueConstraint name cols
                            : getAltersU news old'

    -- Don't drop constraints which were manually added.
    isManual (ConstraintNameDB x) = "__manual_" `T.isPrefixOf` x

getColumn
    :: (Text -> IO Statement)
    -> EntityNameDB
    -> [PersistValue]
    -> Maybe (EntityNameDB, ConstraintNameDB)
    -> IO (Either Text Column)
getColumn getter tableName' [ PersistText columnName
                            , PersistText isNullable
                            , PersistText typeName
                            , defaultValue
                            , generationExpression
                            , numericPrecision
                            , numericScale
                            , maxlen
                            ] refName_ = runExceptT $ do
    defaultValue' <-
        case defaultValue of
            PersistNull ->
                pure Nothing
            PersistText t ->
                pure $ Just t
            _ ->
                throwError $ T.pack $ "Invalid default column: " ++ show defaultValue

    generationExpression' <-
        case generationExpression of
            PersistNull ->
                pure Nothing
            PersistText t ->
                pure $ Just t
            _ ->
                throwError $ T.pack $ "Invalid generated column: " ++ show generationExpression

    let typeStr =
            case maxlen of
                PersistInt64 n ->
                    T.concat [typeName, "(", T.pack (show n), ")"]
                _ ->
                    typeName

    t <- getType typeStr

    let cname = FieldNameDB columnName

    ref <- lift $ fmap join $ traverse (getRef cname) refName_

    return Column
        { cName = cname
        , cNull = isNullable == "YES"
        , cSqlType = t
        , cDefault = fmap stripSuffixes defaultValue'
        , cGenerated = fmap stripSuffixes generationExpression'
        , cDefaultConstraintName = Nothing
        , cMaxLen = Nothing
        , cReference = fmap (\(a,b,c,d) -> ColumnReference a b (mkCascade c d)) ref
        }

  where

    mkCascade updText delText =
        FieldCascade
            { fcOnUpdate = parseCascade updText
            , fcOnDelete = parseCascade delText
            }

    parseCascade txt =
        case txt of
            "NO ACTION" ->
                Nothing
            "CASCADE" ->
                Just Cascade
            "SET NULL" ->
                Just SetNull
            "SET DEFAULT" ->
                Just SetDefault
            "RESTRICT" ->
                Just Restrict
            _ ->
                error $ "Unexpected value in parseCascade: " <> show txt

    stripSuffixes t =
        loop'
            [ "::character varying"
            , "::text"
            ]
      where
        loop' [] = t
        loop' (p:ps) =
            case T.stripSuffix p t of
                Nothing -> loop' ps
                Just t' -> t'

    getRef cname (_, refName') = do
        let sql = T.concat
                [ "SELECT DISTINCT "
                , "ccu.table_name, "
                , "tc.constraint_name, "
                , "rc.update_rule, "
                , "rc.delete_rule "
                , "FROM information_schema.constraint_column_usage ccu "
                , "INNER JOIN information_schema.key_column_usage kcu "
                , "  ON ccu.constraint_name = kcu.constraint_name "
                , "INNER JOIN information_schema.table_constraints tc "
                , "  ON tc.constraint_name = kcu.constraint_name "
                , "LEFT JOIN information_schema.referential_constraints AS rc"
                , "  ON rc.constraint_name = ccu.constraint_name "
                , "WHERE tc.constraint_type='FOREIGN KEY' "
                , "AND kcu.ordinal_position=1 "
                , "AND kcu.table_name=? "
                , "AND kcu.column_name=? "
                , "AND tc.constraint_name=?"
                ]
        stmt <- getter sql
        cntrs <-
            with
                (stmtQuery stmt
                    [ PersistText $ unEntityNameDB tableName'
                    , PersistText $ unFieldNameDB cname
                    , PersistText $ unConstraintNameDB refName'
                    ]
                )
                (\src -> runConduit $ src .| CL.consume)
        case cntrs of
          [] ->
              return Nothing
          [[PersistText table, PersistText constraint, PersistText updRule, PersistText delRule]] ->
              return $ Just (EntityNameDB table, ConstraintNameDB constraint, updRule, delRule)
          xs ->
              error $ mconcat
                  [ "Postgresql.getColumn: error fetching constraints. Expected a single result for foreign key query for table: "
                  , T.unpack (unEntityNameDB tableName')
                  , " and column: "
                  , T.unpack (unFieldNameDB cname)
                  , " but got: "
                  , show xs
                  ]

    getType "int4"        = pure SqlInt32
    getType "int8"        = pure SqlInt64
    getType "varchar"     = pure SqlString
    getType "text"        = pure SqlString
    getType "date"        = pure SqlDay
    getType "bool"        = pure SqlBool
    getType "timestamptz" = pure SqlDayTime
    getType "float4"      = pure SqlReal
    getType "float8"      = pure SqlReal
    getType "bytea"       = pure SqlBlob
    getType "time"        = pure SqlTime
    getType "numeric"     = getNumeric numericPrecision numericScale
    getType a             = pure $ SqlOther a

    getNumeric (PersistInt64 a) (PersistInt64 b) =
        pure $ SqlNumeric (fromIntegral a) (fromIntegral b)

    getNumeric PersistNull PersistNull = throwError $ T.concat
        [ "No precision and scale were specified for the column: "
        , columnName
        , " in table: "
        , unEntityNameDB tableName'
        , ". Postgres defaults to a maximum scale of 147,455 and precision of 16383,"
        , " which is probably not what you intended."
        , " Specify the values as numeric(total_digits, digits_after_decimal_place)."
        ]

    getNumeric a b = throwError $ T.concat
        [ "Can not get numeric field precision for the column: "
        , columnName
        , " in table: "
        , unEntityNameDB tableName'
        , ". Expected an integer for both precision and scale, "
        , "got: "
        , T.pack $ show a
        , " and "
        , T.pack $ show b
        , ", respectively."
        , " Specify the values as numeric(total_digits, digits_after_decimal_place)."
        ]

getColumn _ _ columnName _ =
    return $ Left $ T.pack $ "Invalid result from information_schema: " ++ show columnName

-- | Intelligent comparison of SQL types, to account for SqlInt32 vs SqlOther integer
sqlTypeEq :: SqlType -> SqlType -> Bool
sqlTypeEq x y =
    T.toCaseFold (showSqlType x) == T.toCaseFold (showSqlType y)

findAlters
    :: [EntityDef]
    -- ^ The list of all entity definitions that persistent is aware of.
    -> EntityDef
    -- ^ The entity definition for the entity that we're working on.
    -> Column
    -- ^ The column that we're searching for potential alterations for.
    -> [Column]
    -> ([AlterColumn], [Column])
findAlters defs edef col@(Column name isNull sqltype def _gen _defConstraintName _maxLen ref) cols =
    case List.find (\c -> cName c == name) cols of
        Nothing ->
            ([Add' col], cols)
        Just (Column _oldName isNull' sqltype' def' _gen' _defConstraintName' _maxLen' ref') ->
            let refDrop Nothing = []
                refDrop (Just ColumnReference {crConstraintName=cname}) =
                    [DropReference cname]

                refAdd Nothing = []
                refAdd (Just colRef) =
                    case find ((== crTableName colRef) . getEntityDBName) defs of
                        Just refdef
                            | Just _oldName /= fmap fieldDB (getEntityIdField edef)
                            ->
                            [AddReference
                                (crTableName colRef)
                                (crConstraintName colRef)
                                [name]
                                (NEL.toList $ Util.dbIdColumnsEsc escapeF refdef)
                                (crFieldCascade colRef)
                            ]
                        Just _ -> []
                        Nothing ->
                            error $ "could not find the entityDef for reftable["
                                ++ show (crTableName colRef) ++ "]"
                modRef =
                    if equivalentRef ref ref'
                        then []
                        else refDrop ref' ++ refAdd ref
                modNull = case (isNull, isNull') of
                            (True, False) ->  do
                                guard $ Just name /= fmap fieldDB (getEntityIdField edef)
                                pure (IsNull col)
                            (False, True) ->
                                let up = case def of
                                            Nothing -> id
                                            Just s -> (:) (Update' col s)
                                 in up [NotNull col]
                            _ -> []
                modType
                    | sqlTypeEq sqltype sqltype' = []
                    -- When converting from Persistent pre-2.0 databases, we
                    -- need to make sure that TIMESTAMP WITHOUT TIME ZONE is
                    -- treated as UTC.
                    | sqltype == SqlDayTime && sqltype' == SqlOther "timestamp" =
                        [ChangeType col sqltype $ T.concat
                            [ " USING "
                            , escapeF name
                            , " AT TIME ZONE 'UTC'"
                            ]]
                    | otherwise = [ChangeType col sqltype ""]
                modDef =
                    if def == def'
                        || isJust (T.stripPrefix "nextval" =<< def')
                        then []
                        else
                            case def of
                                Nothing -> [NoDefault col]
                                Just s  -> [Default col s]
                dropSafe =
                    if safeToRemove edef name
                        then error "wtf" [Drop col True]
                        else []
             in
                ( modRef ++ modDef ++ modNull ++ modType ++ dropSafe
                , filter (\c -> cName c /= name) cols
                )

-- We check if we should alter a foreign key. This is almost an equality check,
-- except we consider 'Nothing' and 'Just Restrict' equivalent.
equivalentRef :: Maybe ColumnReference -> Maybe ColumnReference -> Bool
equivalentRef Nothing Nothing = True
equivalentRef (Just cr1) (Just cr2) =
       crTableName cr1 == crTableName cr2
    && crConstraintName cr1 == crConstraintName cr2
    && eqCascade (fcOnUpdate $ crFieldCascade cr1) (fcOnUpdate $ crFieldCascade cr2)
    && eqCascade (fcOnDelete $ crFieldCascade cr1) (fcOnDelete $ crFieldCascade cr2)
  where
    eqCascade :: Maybe CascadeAction -> Maybe CascadeAction -> Bool
    eqCascade Nothing Nothing         = True
    eqCascade Nothing (Just Restrict) = True
    eqCascade (Just Restrict) Nothing = True
    eqCascade (Just cs1) (Just cs2)   = cs1 == cs2
    eqCascade _ _                     = False
equivalentRef _ _ = False

-- | Get the references to be added to a table for the given column.
getAddReference
    :: [EntityDef]
    -> EntityDef
    -> FieldNameDB
    -> ColumnReference
    -> Maybe AlterDB
getAddReference allDefs entity cname cr@ColumnReference {crTableName = s, crConstraintName=constraintName} = do
    guard $ Just cname /= fmap fieldDB (getEntityIdField entity)
    pure $ AlterColumn
        table
        (AddReference s constraintName [cname] id_ (crFieldCascade cr)
        )
  where
    table = getEntityDBName entity
    id_ =
        fromMaybe
            (error $ "Could not find ID of entity " ++ show s)
            $ do
                entDef <- find ((== s) . getEntityDBName) allDefs
                return $ NEL.toList $ Util.dbIdColumnsEsc escapeF entDef

showColumn :: Column -> Text
showColumn (Column n nu sqlType' def gen _defConstraintName _maxLen _ref) = T.concat
    [ escapeF n
    , " "
    , showSqlType sqlType'
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s -> " DEFAULT " <> s
    , case gen of
        Nothing -> ""
        Just s -> " GENERATED ALWAYS AS (" <> s <> ") STORED"
    ]

showSqlType :: SqlType -> Text
showSqlType SqlString = "VARCHAR"
showSqlType SqlInt32 = "INT4"
showSqlType SqlInt64 = "INT8"
showSqlType SqlReal = "DOUBLE PRECISION"
showSqlType (SqlNumeric s prec) = T.concat [ "NUMERIC(", T.pack (show s), ",", T.pack (show prec), ")" ]
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP WITH TIME ZONE"
showSqlType SqlBlob = "BYTEA"
showSqlType SqlBool = "BOOLEAN"

-- Added for aliasing issues re: https://github.com/yesodweb/yesod/issues/682
showSqlType (SqlOther (T.toLower -> "integer")) = "INT4"

showSqlType (SqlOther t) = t

showAlterDb :: AlterDB -> (Bool, Text)
showAlterDb (AddTable s) = (False, s)
showAlterDb (AlterColumn t ac) =
    (isUnsafe ac, showAlter t ac)
  where
    isUnsafe (Drop _ safeRemove) = not safeRemove
    isUnsafe _ = False
showAlterDb (AlterTable t at) = (False, showAlterTable t at)

showAlterTable :: EntityNameDB -> AlterTable -> Text
showAlterTable table (AddUniqueConstraint cname cols) = T.concat
    [ "ALTER TABLE "
    , escapeE table
    , " ADD CONSTRAINT "
    , escapeC cname
    , " UNIQUE("
    , T.intercalate "," $ map escapeF cols
    , ")"
    ]
showAlterTable table (DropConstraint cname) = T.concat
    [ "ALTER TABLE "
    , escapeE table
    , " DROP CONSTRAINT "
    , escapeC cname
    ]

showAlter :: EntityNameDB -> AlterColumn -> Text
showAlter table (ChangeType c t extra) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " TYPE "
        , showSqlType t
        , extra
        ]
showAlter table (IsNull c) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " DROP NOT NULL"
        ]
showAlter table (NotNull c) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " SET NOT NULL"
        ]
showAlter table (Add' col) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ADD COLUMN "
        , showColumn col
        ]
showAlter table (Drop c _) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " DROP COLUMN "
        , escapeF (cName c)
        ]
showAlter table (Default c s) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " SET DEFAULT "
        , s
        ]
showAlter table (NoDefault c) = T.concat
    [ "ALTER TABLE "
    , escapeE table
    , " ALTER COLUMN "
    , escapeF (cName c)
    , " DROP DEFAULT"
    ]
showAlter table (Update' c s) = T.concat
    [ "UPDATE "
    , escapeE table
    , " SET "
    , escapeF (cName c)
    , "="
    , s
    , " WHERE "
    , escapeF (cName c)
    , " IS NULL"
    ]
showAlter table (AddReference reftable fkeyname t2 id2 cascade) = T.concat
    [ "ALTER TABLE "
    , escapeE table
    , " ADD CONSTRAINT "
    , escapeC fkeyname
    , " FOREIGN KEY("
    , T.intercalate "," $ map escapeF t2
    , ") REFERENCES "
    , escapeE reftable
    , "("
    , T.intercalate "," id2
    , ")"
    ] <> renderFieldCascade cascade
showAlter table (DropReference cname) = T.concat
    [ "ALTER TABLE "
    , escapeE table
    , " DROP CONSTRAINT "
    , escapeC cname
    ]

-- | Get the SQL string for the table that a PeristEntity represents.
-- Useful for raw SQL queries.
tableName :: (PersistEntity record) => record -> Text
tableName = escapeE . tableDBName

-- | Get the SQL string for the field that an EntityField represents.
-- Useful for raw SQL queries.
fieldName :: (PersistEntity record) => EntityField record typ -> Text
fieldName = escapeF . fieldDBName

escapeC :: ConstraintNameDB -> Text
escapeC = escapeWith escape

escapeE :: EntityNameDB -> Text
escapeE = escapeWith escape

escapeF :: FieldNameDB -> Text
escapeF = escapeWith escape


escape :: Text -> Text
escape s =
    T.pack $ '"' : go (T.unpack s) ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs

-- | Information required to connect to a PostgreSQL database
-- using @persistent@'s generic facilities.  These values are the
-- same that are given to 'withPostgresqlPool'.
data PostgresConf = PostgresConf
    { pgConnStr  :: ConnectionString
      -- ^ The connection string.

    -- TODO: Currently stripes, idle timeout, and pool size are all separate fields
    -- When Persistent next does a large breaking release (3.0?), we should consider making these just a single ConnectionPoolConfig value
    --
    -- Currently there the idle timeout is an Integer, rather than resource-pool's NominalDiffTime type.
    -- This is because the time package only recently added the Read instance for NominalDiffTime.
    -- Future TODO: Consider removing the Read instance, and/or making the idle timeout a NominalDiffTime.

    , pgPoolStripes :: Int
    -- ^ How many stripes to divide the pool into. See "Data.Pool" for details.
    -- @since 2.11.0.0
    , pgPoolIdleTimeout :: Integer -- Ideally this would be a NominalDiffTime, but that type lacks a Read instance https://github.com/haskell/time/issues/130
    -- ^ How long connections can remain idle before being disposed of, in seconds.
    -- @since 2.11.0.0
    , pgPoolSize :: Int
      -- ^ How many connections should be held in the connection pool.
    } deriving (Show, Read, Data)

instance FromJSON PostgresConf where
    parseJSON v = modifyFailure ("Persistent: error loading PostgreSQL conf: " ++) $
      flip (withObject "PostgresConf") v $ \o -> do
        let defaultPoolConfig = defaultConnectionPoolConfig
        database <- o .: "database"
        host     <- o .: "host"
        port     <- o .:? "port" .!= 5432
        user     <- o .: "user"
        password <- o .: "password"
        poolSize <- o .:? "poolsize" .!= (connectionPoolConfigSize defaultPoolConfig)
        poolStripes <- o .:? "stripes" .!= (connectionPoolConfigStripes defaultPoolConfig)
        poolIdleTimeout <- o .:? "idleTimeout" .!= (floor $ connectionPoolConfigIdleTimeout defaultPoolConfig)
        let ci = PG.ConnectInfo
                   { PG.connectHost     = host
                   , PG.connectPort     = port
                   , PG.connectUser     = user
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
        return $ addUser env
               $ addPass env
               $ addDatabase env
               $ addPort env
               $ addHost env c0
      where
        addParam param val c =
            c { pgConnStr = B8.concat [pgConnStr c, " ", param, "='", pgescape val, "'"] }

        pgescape = B8.pack . go
            where
              go ('\'':rest) = '\\' : '\'' : go rest
              go ('\\':rest) = '\\' : '\\' : go rest
              go ( x  :rest) =      x      : go rest
              go []          = []

        maybeAddParam param envvar env =
            maybe id (addParam param) $
            lookup envvar env

        addHost     = maybeAddParam "host"     "PGHOST"
        addPort     = maybeAddParam "port"     "PGPORT"
        addUser     = maybeAddParam "user"     "PGUSER"
        addPass     = maybeAddParam "password" "PGPASS"
        addDatabase = maybeAddParam "dbname"   "PGDATABASE"

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
defaultPostgresConfHooks = PostgresConfHooks
  { pgConfHooksGetServerVersion = getServerVersionNonEmpty
  , pgConfHooksAfterCreate = const $ pure ()
  }


refName :: EntityNameDB -> FieldNameDB -> ConstraintNameDB
refName (EntityNameDB table) (FieldNameDB column) =
    let overhead = T.length $ T.concat ["_", "_fkey"]
        (fromTable, fromColumn) = shortenNames overhead (T.length table, T.length column)
    in ConstraintNameDB $ T.concat [T.take fromTable table, "_", T.take fromColumn column, "_fkey"]

    where

      -- Postgres automatically truncates too long foreign keys to a combination of
      -- truncatedTableName + "_" + truncatedColumnName + "_fkey"
      -- This works fine for normal use cases, but it creates an issue for Persistent
      -- Because after running the migrations, Persistent sees the truncated foreign key constraint
      -- doesn't have the expected name, and suggests that you migrate again
      -- To workaround this, we copy the Postgres truncation approach before sending foreign key constraints to it.
      --
      -- I believe this will also be an issue for extremely long table names,
      -- but it's just much more likely to exist with foreign key constraints because they're usually tablename * 2 in length

      -- Approximation of the algorithm Postgres uses to truncate identifiers
      -- See makeObjectName https://github.com/postgres/postgres/blob/5406513e997f5ee9de79d4076ae91c04af0c52f6/src/backend/commands/indexcmds.c#L2074-L2080
      shortenNames :: Int -> (Int, Int) -> (Int, Int)
      shortenNames overhead (x, y)
           | x + y + overhead <= maximumIdentifierLength = (x, y)
           | x > y = shortenNames overhead (x - 1, y)
           | otherwise = shortenNames overhead (x, y - 1)

-- | Postgres' default maximum identifier length in bytes
-- (You can re-compile Postgres with a new limit, but I'm assuming that virtually noone does this).
-- See https://www.postgresql.org/docs/11/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
maximumIdentifierLength :: Int
maximumIdentifierLength = 63

udToPair :: UniqueDef -> (ConstraintNameDB, [FieldNameDB])
udToPair ud = (uniqueDBName ud, map snd $ NEL.toList $ uniqueFields ud)

mockMigrate :: [EntityDef]
         -> (Text -> IO Statement)
         -> EntityDef
         -> IO (Either [Text] [(Bool, Text)])
mockMigrate allDefs _ entity = fmap (fmap $ map showAlterDb) $ do
    case partitionEithers [] of
        ([], old'') -> return $ Right $ migrationText False old''
        (errs, _) -> return $ Left errs
  where
    name = getEntityDBName entity
    migrationText exists' old'' =
        if not exists'
            then createText newcols fdefs udspair
            else let (acs, ats) = getAlters allDefs entity (newcols, udspair) old'
                     acs' = map (AlterColumn name) acs
                     ats' = map (AlterTable name) ats
                 in  acs' ++ ats'
       where
         old' = partitionEithers old''
         (newcols', udefs, fdefs) = postgresMkColumns allDefs entity
         newcols = filter (not . safeToRemove entity . cName) newcols'
         udspair = map udToPair udefs
            -- Check for table existence if there are no columns, workaround
            -- for https://github.com/yesodweb/persistent/issues/152

    createText newcols fdefs udspair =
        (addTable newcols entity) : uniques ++ references ++ foreignsAlt
      where
        uniques = flip concatMap udspair $ \(uname, ucols) ->
                [AlterTable name $ AddUniqueConstraint uname ucols]
        references =
            mapMaybe
                (\Column { cName, cReference } ->
                    getAddReference allDefs entity cName =<< cReference
                )
                newcols
        foreignsAlt = mapMaybe (mkForeignAlt entity) fdefs

-- | Mock a migration even when the database is not present.
-- This function performs the same functionality of 'printMigration'
-- with the difference that an actual database is not needed.
mockMigration :: Migration -> IO ()
mockMigration mig = do
    smap <- newIORef $ Map.empty
    let sqlbackend =
            mkSqlBackend MkSqlBackendArgs
                { connPrepare = \_ -> do
                    return Statement
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
    conflictColumns = concatMap (map (escapeF . snd) . NEL.toList . uniqueFields) (getEntityUniques ent)

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
  CopyUnlessEq :: PersistField typ => EntityField record typ -> typ -> HandleUpdateCollision record

-- | Copy the field into the database only if the value in the
-- corresponding record is non-@NULL@.
--
-- @since  2.12.1.0
copyUnlessNull :: PersistField typ => EntityField record (Maybe typ) -> HandleUpdateCollision record
copyUnlessNull field = CopyUnlessEq field Nothing

-- | Copy the field into the database only if the value in the
-- corresponding record is non-empty, where "empty" means the Monoid
-- definition for 'mempty'. Useful for 'Text', 'String', 'ByteString', etc.
--
-- The resulting 'HandleUpdateCollision' type is useful for the
-- 'upsertManyWhere' function.
--
-- @since  2.12.1.0
copyUnlessEmpty :: (Monoid.Monoid typ, PersistField typ) => EntityField record typ -> HandleUpdateCollision record
copyUnlessEmpty field = CopyUnlessEq field Monoid.mempty

-- | Copy the field into the database only if the field is not equal to the
-- provided value. This is useful to avoid copying weird nullary data into
-- the database.
--
-- The resulting 'HandleUpdateCollision' type is useful for the
-- 'upsertMany' function.
--
-- @since  2.12.1.0
copyUnlessEq :: PersistField typ => EntityField record typ -> typ -> HandleUpdateCollision record
copyUnlessEq = CopyUnlessEq

-- | Copy the field directly from the record.
--
-- @since 2.12.1.0
copyField :: PersistField typ => EntityField record typ -> HandleUpdateCollision record
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

-- | Postgres specific 'upsertManyWhere'. This method does the following:
-- It will insert a record if no matching unique key exists.
-- If a unique key exists, it will update the relevant field with a user-supplied value, however,
-- it will only do this update on a user-supplied condition.
-- For example, here's how this method could be called like such:
--
-- upsertManyWhere [record] [recordField =. newValue] [recordField !=. newValue]
--
-- Called thusly, this method will insert a new record (if none exists) OR update a recordField with a new value
-- assuming the condition in the last block is met.
--
-- @since 2.12.1.0
upsertManyWhere
    :: forall record backend m.
    ( backend ~ PersistEntityBackend record
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
    let uniqDef = onlyOneUniqueDef (Proxy :: Proxy record)
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
        T.encodeUtf8
            $ "EXCLUDED."
            <> fieldName field

-- | This creates the query for 'upsertManyWhere'. If you
-- provide an empty list of updates to perform, then it will generate
-- a dummy/no-op update using the first field of the record. This avoids
-- duplicate key exceptions.
mkBulkUpsertQuery
    :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, OnlyOneUniqueKey record)
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
        (field:_) -> field
    entityFieldNames = map fieldDbToText (getEntityFields entityDef')
    nameOfTable = escapeE . getEntityDBName $ entityDef'
    copyUnlessValues = map snd fieldsToMaybeCopy
    recordValues = concatMap (map toPersistValue . toPersistFields) records
    recordPlaceholders =
        Util.commaSeparated
        $ map (Util.parenWrapped . Util.commaSeparated . map (const "?") . toPersistFields)
        $ records
    mkCondFieldSet n _ =
        T.concat
            [ n
            , "=COALESCE("
            ,   "NULLIF("
            ,     "EXCLUDED."
            ,       n
            ,         ","
            ,           "?"
            ,         ")"
            ,       ","
            ,     nameOfTable
            ,   "."
            ,   n
            ,")"
            ]
    condFieldSets = map (uncurry mkCondFieldSet) fieldsToMaybeCopy
    fieldSets = map (\n -> T.concat [n, "=EXCLUDED.", n, ""]) updateFieldNames
    upds = map (Util.mkUpdateText' (escapeF) (\n -> T.concat [nameOfTable, ".", n])) updates
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
    q = T.concat
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


-- | Enable a Postgres extension. See https://www.postgresql.org/docs/current/static/contrib.html
-- for a list.
migrateEnableExtension :: Text -> Migration
migrateEnableExtension extName = WriterT $ WriterT $ do
  res :: [Single Int] <-
    rawSql "SELECT COUNT(*) FROM pg_catalog.pg_extension WHERE extname = ?" [PersistText extName]
  if res == [Single 0]
    then return (((), []) , [(False, "CREATe EXTENSION \"" <> extName <> "\"")])
    else return (((), []), [])

postgresMkColumns :: [EntityDef] -> EntityDef -> ([Column], [UniqueDef], [ForeignDef])
postgresMkColumns allDefs t =
    mkColumns allDefs t
    $ setBackendSpecificForeignKeyName refName emptyBackendSpecificOverrides

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

instance BackendCompatible b (RawPostgresql b) where
    projectBackend = persistentBackend

withRawConnection
    :: (PG.Connection -> SqlBackend)
    -> PG.Connection
    -> RawPostgresql SqlBackend
withRawConnection f conn = RawPostgresql
    { persistentBackend = f conn
    , rawPostgresqlConnection = conn
    }

-- | Create a PostgreSQL connection pool which also exposes the
-- raw connection. The raw counterpart to 'createPostgresqlPool'.
--
-- @since 2.13.1.0
createRawPostgresqlPool :: (MonadUnliftIO m, MonadLoggerIO m)
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
    => (PG.Connection -> IO ()) -- ^ Action to perform after connection is created.
    -> ConnectionString -- ^ Connection string to the database.
    -> Int -- ^ Number of connections to be kept open in the pool.
    -> m (Pool (RawPostgresql SqlBackend))
createRawPostgresqlPoolModified = createRawPostgresqlPoolModifiedWithVersion getServerVersion

-- | The raw counterpart to 'createPostgresqlPoolModifiedWithVersion'.
--
-- @since 2.13.1.0
createRawPostgresqlPoolModifiedWithVersion
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => (PG.Connection -> IO (Maybe Double)) -- ^ Action to perform to get the server version.
    -> (PG.Connection -> IO ()) -- ^ Action to perform after connection is created.
    -> ConnectionString -- ^ Connection string to the database.
    -> Int -- ^ Number of connections to be kept open in the pool.
    -> m (Pool (RawPostgresql SqlBackend))
createRawPostgresqlPoolModifiedWithVersion getVerDouble modConn ci = do
  let getVer = oldGetVersionToNew getVerDouble
  createSqlPool $ open' modConn getVer withRawConnection ci

-- | The raw counterpart to 'createPostgresqlPoolWithConf'.
--
-- @since 2.13.1.0
createRawPostgresqlPoolWithConf
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => PostgresConf -- ^ Configuration for connecting to Postgres
    -> PostgresConfHooks -- ^ Record of callback functions
    -> m (Pool (RawPostgresql SqlBackend))
createRawPostgresqlPoolWithConf conf hooks = do
  let getVer = pgConfHooksGetServerVersion hooks
      modConn = pgConfHooksAfterCreate hooks
  createSqlPoolWithConfig (open' modConn getVer withRawConnection (pgConnStr conf)) (postgresConfToConnectionPoolConfig conf)

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

