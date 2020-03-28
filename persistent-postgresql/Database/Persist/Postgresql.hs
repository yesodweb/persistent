{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A postgresql backend for persistent.
module Database.Persist.Postgresql
    ( withPostgresqlPool
    , withPostgresqlPoolWithVersion
    , withPostgresqlConn
    , withPostgresqlConnWithVersion
    , createPostgresqlPool
    , createPostgresqlPoolModified
    , createPostgresqlPoolModifiedWithVersion
    , module Database.Persist.Sql
    , ConnectionString
    , PostgresConf (..)
    , PgInterval (..)
    , openSimpleConn
    , openSimpleConnWithVersion
    , tableName
    , fieldName
    , mockMigration
    , migrateEnableExtension
    ) where

import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Internal as PG
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as PS
import Database.PostgreSQL.Simple.Ok (Ok (..))

import Control.Arrow
import Control.Exception (Exception, throw, throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT)

import qualified Blaze.ByteString.Builder.Char8 as BBB
import Data.Acquire (Acquire, mkAcquire, with)
import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data
import Data.Either (partitionEithers)
import Data.Fixed (Fixed(..), Pico)
import Data.Function (on)
import Data.Int (Int64)
import qualified Data.IntMap as I
import Data.IORef
import Data.List (find, sort, groupBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text.Read (rational)
import Data.Time (utc, NominalDiffTime, localTimeToUTC)
import Data.Typeable (Typeable)
import System.Environment (getEnvironment)

import Database.Persist.Sql
import qualified Database.Persist.Sql.Util as Util

-- | A @libpq@ connection string.  A simple example of connection
-- string would be @\"host=localhost port=5432 user=test
-- dbname=test password=test\"@.  Please read libpq's
-- documentation at
-- <https://www.postgresql.org/docs/current/static/libpq-connect.html>
-- for more details on how to create such strings.
type ConnectionString = ByteString

-- | PostgresServerVersionError exception. This is thrown when persistent
-- is unable to find the version of the postgreSQL server.
data PostgresServerVersionError = PostgresServerVersionError String deriving Data.Typeable.Typeable

instance Show PostgresServerVersionError where
    show (PostgresServerVersionError uniqueMsg) =
      "Unexpected PostgreSQL server version, got " <> uniqueMsg
instance Exception PostgresServerVersionError

-- | Create a PostgreSQL connection pool and run the given action.  The pool is
-- properly released after the action finishes using it.  Note that you should
-- not use the given 'ConnectionPool' outside the action since it may already
-- have been released.
-- The provided action should use 'runSqlConn' and *not* 'runReaderT' because
-- the former brackets the database action with transaction begin/commit.
withPostgresqlPool :: (MonadLogger m, MonadUnliftIO m)
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
withPostgresqlPoolWithVersion :: (MonadUnliftIO m, MonadLogger m)
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
withPostgresqlPoolWithVersion getVer ci = withSqlPool $ open' (const $ return ()) getVer ci

-- | Create a PostgreSQL connection pool.  Note that it's your
-- responsibility to properly close the connection pool when
-- unneeded.  Use 'withPostgresqlPool' for an automatic resource
-- control.
createPostgresqlPool :: (MonadUnliftIO m, MonadLogger m)
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
    :: (MonadUnliftIO m, MonadLogger m)
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
    :: (MonadUnliftIO m, MonadLogger m)
    => (PG.Connection -> IO (Maybe Double)) -- ^ Action to perform to get the server version.
    -> (PG.Connection -> IO ()) -- ^ Action to perform after connection is created.
    -> ConnectionString -- ^ Connection string to the database.
    -> Int -- ^ Number of connections to be kept open in the pool.
    -> m (Pool SqlBackend)
createPostgresqlPoolModifiedWithVersion getVer modConn ci =
  createSqlPool $ open' modConn getVer ci

-- | Same as 'withPostgresqlPool', but instead of opening a pool
-- of connections, only one connection is opened.
-- The provided action should use 'runSqlConn' and *not* 'runReaderT' because
-- the former brackets the database action with transaction begin/commit.
withPostgresqlConn :: (MonadUnliftIO m, MonadLogger m)
                   => ConnectionString -> (SqlBackend -> m a) -> m a
withPostgresqlConn = withPostgresqlConnWithVersion getServerVersion

-- | Same as 'withPostgresqlConn', but takes a callback for obtaining
-- the server version (to work around an Amazon Redshift bug).
--
-- @since 2.6.2
withPostgresqlConnWithVersion :: (MonadUnliftIO m, MonadLogger m)
                              => (PG.Connection -> IO (Maybe Double))
                              -> ConnectionString
                              -> (SqlBackend -> m a)
                              -> m a
withPostgresqlConnWithVersion getVer = withSqlConn . open' (const $ return ()) getVer

open'
    :: (PG.Connection -> IO ())
    -> (PG.Connection -> IO (Maybe Double))
    -> ConnectionString -> LogFunc -> IO SqlBackend
open' modConn getVer cstr logFunc = do
    conn <- PG.connectPostgreSQL cstr
    modConn conn
    ver <- getVer conn
    smap <- newIORef $ Map.empty
    return $ createBackend logFunc ver smap conn

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

-- | Choose upsert sql generation function based on postgresql version.
-- PostgreSQL version >= 9.5 supports native upsert feature,
-- so depending upon that we have to choose how the sql query is generated.
-- upsertFunction :: Double -> Maybe (EntityDef -> Text -> Text)
upsertFunction :: a -> Double -> Maybe a
upsertFunction f version = if (version >= 9.5)
                         then Just f
                         else Nothing


-- | Generate a 'SqlBackend' from a 'PG.Connection'.
openSimpleConn :: LogFunc -> PG.Connection -> IO SqlBackend
openSimpleConn = openSimpleConnWithVersion getServerVersion

-- | Generate a 'SqlBackend' from a 'PG.Connection', but takes a callback for
-- obtaining the server version.
--
-- @since 2.9.1
openSimpleConnWithVersion :: (PG.Connection -> IO (Maybe Double)) -> LogFunc -> PG.Connection -> IO SqlBackend
openSimpleConnWithVersion getVer logFunc conn = do
    smap <- newIORef $ Map.empty
    serverVersion <- getVer conn
    return $ createBackend logFunc serverVersion smap conn

-- | Create the backend given a logging function, server version, mutable statement cell,
-- and connection.
createBackend :: LogFunc -> Maybe Double
              -> IORef (Map.Map Text Statement) -> PG.Connection -> SqlBackend
createBackend logFunc serverVersion smap conn = do
    SqlBackend
        { connPrepare    = prepare' conn
        , connStmtMap    = smap
        , connInsertSql  = insertSql'
        , connInsertManySql = Just insertManySql'
        , connUpsertSql  = serverVersion >>= upsertFunction upsertSql'
        , connPutManySql = serverVersion >>= upsertFunction putManySql
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
        , connEscapeName = escape
        , connNoLimit    = "LIMIT ALL"
        , connRDBMS      = "postgresql"
        , connLimitOffset = decorateSQLWithLimitOffset "LIMIT ALL"
        , connLogFunc = logFunc
        , connMaxParams = Nothing
        , connRepsertManySql = serverVersion >>= upsertFunction repsertManySql
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
  let sql = T.concat
                [ "INSERT INTO "
                , escape $ entityDB ent
                , if null (entityFields ent)
                    then " DEFAULT VALUES"
                    else T.concat
                        [ "("
                        , T.intercalate "," $ map (escape . fieldDB) $ entityFields ent
                        , ") VALUES("
                        , T.intercalate "," (map (const "?") $ entityFields ent)
                        , ")"
                        ]
                ]
  in case entityPrimary ent of
       Just _pdef -> ISRManyKeys sql vals
       Nothing -> ISRSingle (sql <> " RETURNING " <> escape (fieldDB (entityId ent)))


upsertSql' :: EntityDef -> NonEmpty UniqueDef -> Text -> Text
upsertSql' ent uniqs updateVal = T.concat
                           [ "INSERT INTO "
                           , escape (entityDB ent)
                           , "("
                           , T.intercalate "," $ map (escape . fieldDB) $ entityFields ent
                           , ") VALUES ("
                           , T.intercalate "," $ map (const "?") (entityFields ent)
                           , ") ON CONFLICT ("
                           , T.intercalate "," $ concat $ map (\x -> map escape (map snd $ uniqueFields x)) (entityUniques ent)
                           , ") DO UPDATE SET "
                           , updateVal
                           , " WHERE "
                           , wher
                           , " RETURNING ??"
                           ]
    where
      wher = T.intercalate " AND " $ map singleCondition $ NEL.toList uniqs

      singleCondition :: UniqueDef -> Text
      singleCondition udef = T.intercalate " AND " (map singleClause $ map snd (uniqueFields udef))

      singleClause :: DBName -> Text
      singleClause field = escape (entityDB ent) <> "." <> (escape field) <> " =?"

-- | SQL for inserting multiple rows at once and returning their primary keys.
insertManySql' :: EntityDef -> [[PersistValue]] -> InsertSqlResult
insertManySql' ent valss =
  let sql = T.concat
                [ "INSERT INTO "
                , escape (entityDB ent)
                , "("
                , T.intercalate "," $ map (escape . fieldDB) $ entityFields ent
                , ") VALUES ("
                , T.intercalate "),(" $ replicate (length valss) $ T.intercalate "," $ map (const "?") (entityFields ent)
                , ") RETURNING "
                , Util.commaSeparated $ Util.dbIdColumnsEsc escape ent
                ]
  in ISRSingle sql

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
            = map (\(col, oid) -> getGetter conn oid $ PG.Field rt col oid) ids
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

-- | Avoid orphan instances.
newtype P = P PersistValue


instance PGTF.ToField P where
    toField (P (PersistText t))        = PGTF.toField t
    toField (P (PersistByteString bs)) = PGTF.toField (PG.Binary bs)
    toField (P (PersistInt64 i))       = PGTF.toField i
    toField (P (PersistDouble d))      = PGTF.toField d
    toField (P (PersistRational r))    = PGTF.Plain $
                                         BBB.fromString $
                                         show (fromRational r :: Pico) --  FIXME: Too Ambigous, can not select precision without information about field
    toField (P (PersistBool b))        = PGTF.toField b
    toField (P (PersistDay d))         = PGTF.toField d
    toField (P (PersistTimeOfDay t))   = PGTF.toField t
    toField (P (PersistUTCTime t))     = PGTF.toField t
    toField (P PersistNull)            = PGTF.toField PG.Null
    toField (P (PersistList l))        = PGTF.toField $ listToJSON l
    toField (P (PersistMap m))         = PGTF.toField $ mapToJSON m
    toField (P (PersistDbSpecific s))  = PGTF.toField (Unknown s)
    toField (P (PersistArray a))       = PGTF.toField $ PG.PGArray $ P <$> a
    toField (P (PersistObjectId _))    =
        error "Refusing to serialize a PersistObjectId to a PostgreSQL value"

-- | Represent Postgres interval using NominalDiffTime
--
-- @since 2.11.0.0
newtype PgInterval = PgInterval { getPgInterval :: NominalDiffTime }
  deriving (Eq, Show)

pgIntervalToBs :: PgInterval -> ByteString
pgIntervalToBs = toStrictByteString . show . getPgInterval

instance PGTF.ToField PgInterval where
    toField (PgInterval t) = PGTF.toField t

instance PGFF.FromField PgInterval where
    fromField f mdata =
      if PGFF.typeOid f /= PS.typoid PS.interval
        then PGFF.returnError PGFF.Incompatible f ""
        else case mdata of
          Nothing  -> PGFF.returnError PGFF.UnexpectedNull f ""
          Just dat -> case P.parseOnly (nominalDiffTime <* P.endOfInput) dat of
            Left msg  ->  PGFF.returnError PGFF.ConversionFailed f msg
            Right t   -> return $ PgInterval t

      where
        toPico :: Integer -> Pico
        toPico = MkFixed

        -- Taken from Database.PostgreSQL.Simple.Time.Internal.Parser
        twoDigits :: P.Parser Int
        twoDigits = do
          a <- P.digit
          b <- P.digit
          let c2d c = ord c .&. 15
          return $! c2d a * 10 + c2d b

        -- Taken from Database.PostgreSQL.Simple.Time.Internal.Parser
        seconds :: P.Parser Pico
        seconds = do
          real <- twoDigits
          mc <- P.peekChar
          case mc of
            Just '.' -> do
              t <- P.anyChar *> P.takeWhile1 P.isDigit
              return $! parsePicos (fromIntegral real) t
            _ -> return $! fromIntegral real
         where
          parsePicos :: Int64 -> B8.ByteString -> Pico
          parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
            where n  = max 0 (12 - B8.length t)
                  t' = B8.foldl' (\a c -> 10 * a + fromIntegral (ord c .&. 15)) a0
                                 (B8.take 12 t)

        parseSign :: P.Parser Bool
        parseSign = P.choice [P.char '-' >> return True, return False]

        -- Db stores it in [-]HHH:MM:SS.[SSSS]
        -- For example, nominalDay is stored as 24:00:00
        interval :: P.Parser (Bool, Int, Int, Pico)
        interval = do
            s  <- parseSign
            h  <- P.decimal <* P.char ':'
            m  <- twoDigits <* P.char ':'
            ss <- seconds
            if m < 60 && ss <= 60
                then return (s, h, m, ss)
                else fail "Invalid interval"

        nominalDiffTime :: P.Parser NominalDiffTime
        nominalDiffTime = do
          (s, h, m, ss) <- interval
          let pico   = ss + 60 * (fromIntegral m) + 60 * 60 * (fromIntegral (abs h))
          return . fromRational . toRational $ if s then (-pico) else pico

fromPersistValueError :: Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                      -> Text -- ^ Database type(s), should appear different from Haskell name, e.g. "integer" or "INT", not "Int".
                      -> PersistValue -- ^ Incorrect value
                      -> Text -- ^ Error message
fromPersistValueError haskellType databaseType received = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`; expected "
    , databaseType
    , " from database, but received: "
    , T.pack (show received)
    , ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

instance PersistField PgInterval where
    toPersistValue = PersistDbSpecific . pgIntervalToBs
    fromPersistValue x@(PersistDbSpecific bs) =
      case P.parseOnly (P.signed P.rational <* P.char 's' <* P.endOfInput) bs of
        Left _  -> Left $ fromPersistValueError "PgInterval" "Interval" x
        Right i -> Right $ PgInterval i
    fromPersistValue x = Left $ fromPersistValueError "PgInterval" "Interval" x

instance PersistFieldSql PgInterval where
  sqlType _ = SqlOther "interval"

newtype Unknown = Unknown { unUnknown :: ByteString }
  deriving (Eq, Show, Read, Ord, Typeable)

instance PGFF.FromField Unknown where
    fromField f mdata =
      case mdata of
        Nothing  -> PGFF.returnError PGFF.UnexpectedNull f "Database.Persist.Postgresql/PGFF.FromField Unknown"
        Just dat -> return (Unknown dat)

instance PGTF.ToField Unknown where
    toField (Unknown a) = PGTF.Escape a

type Getter a = PGFF.FieldParser a

convertPV :: PGFF.FromField a => (a -> b) -> Getter b
convertPV f = (fmap f .) . PGFF.fromField

builtinGetters :: I.IntMap (Getter PersistValue)
builtinGetters = I.fromList
    [ (k PS.bool,        convertPV PersistBool)
    , (k PS.bytea,       convertPV (PersistByteString . unBinary))
    , (k PS.char,        convertPV PersistText)
    , (k PS.name,        convertPV PersistText)
    , (k PS.int8,        convertPV PersistInt64)
    , (k PS.int2,        convertPV PersistInt64)
    , (k PS.int4,        convertPV PersistInt64)
    , (k PS.text,        convertPV PersistText)
    , (k PS.xml,         convertPV PersistText)
    , (k PS.float4,      convertPV PersistDouble)
    , (k PS.float8,      convertPV PersistDouble)
    , (k PS.money,       convertPV PersistRational)
    , (k PS.bpchar,      convertPV PersistText)
    , (k PS.varchar,     convertPV PersistText)
    , (k PS.date,        convertPV PersistDay)
    , (k PS.time,        convertPV PersistTimeOfDay)
    , (k PS.timestamp,   convertPV (PersistUTCTime. localTimeToUTC utc))
    , (k PS.timestamptz, convertPV PersistUTCTime)
    , (k PS.interval,    convertPV (PersistDbSpecific . pgIntervalToBs))
    , (k PS.bit,         convertPV PersistInt64)
    , (k PS.varbit,      convertPV PersistInt64)
    , (k PS.numeric,     convertPV PersistRational)
    , (k PS.void,        \_ _ -> return PersistNull)
    , (k PS.json,        convertPV (PersistByteString . unUnknown))
    , (k PS.jsonb,       convertPV (PersistByteString . unUnknown))
    , (k PS.unknown,     convertPV (PersistByteString . unUnknown))

    -- Array types: same order as above.
    -- The OIDs were taken from pg_type.
    , (1000,             listOf PersistBool)
    , (1001,             listOf (PersistByteString . unBinary))
    , (1002,             listOf PersistText)
    , (1003,             listOf PersistText)
    , (1016,             listOf PersistInt64)
    , (1005,             listOf PersistInt64)
    , (1007,             listOf PersistInt64)
    , (1009,             listOf PersistText)
    , (143,              listOf PersistText)
    , (1021,             listOf PersistDouble)
    , (1022,             listOf PersistDouble)
    , (1023,             listOf PersistUTCTime)
    , (1024,             listOf PersistUTCTime)
    , (791,              listOf PersistRational)
    , (1014,             listOf PersistText)
    , (1015,             listOf PersistText)
    , (1182,             listOf PersistDay)
    , (1183,             listOf PersistTimeOfDay)
    , (1115,             listOf PersistUTCTime)
    , (1185,             listOf PersistUTCTime)
    , (1187,             listOf (PersistDbSpecific . pgIntervalToBs))
    , (1561,             listOf PersistInt64)
    , (1563,             listOf PersistInt64)
    , (1231,             listOf PersistRational)
    -- no array(void) type
    , (2951,             listOf (PersistDbSpecific . unUnknown))
    , (199,              listOf (PersistByteString . unUnknown))
    , (3807,             listOf (PersistByteString . unUnknown))
    -- no array(unknown) either
    ]
    where
        k (PGFF.typoid -> i) = PG.oid2int i
        -- A @listOf f@ will use a @PGArray (Maybe T)@ to convert
        -- the values to Haskell-land.  The @Maybe@ is important
        -- because the usual way of checking NULLs
        -- (c.f. withStmt') won't check for NULL inside
        -- arrays---or any other compound structure for that matter.
        listOf f = convertPV (PersistList . map (nullable f) . PG.fromPGArray)
          where nullable = maybe PersistNull

getGetter :: PG.Connection -> PG.Oid -> Getter PersistValue
getGetter _conn oid
  = fromMaybe defaultGetter $ I.lookup (PG.oid2int oid) builtinGetters
  where defaultGetter = convertPV (PersistDbSpecific . unUnknown)

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

doesTableExist :: (Text -> IO Statement)
               -> DBName -- ^ table name
               -> IO Bool
doesTableExist getter (DBName name) = do
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
            exists <-
                if null old
                    then doesTableExist getter name
                    else return True
            return $ Right $ migrationText exists old''
        (errs, _) -> return $ Left errs
  where
    name = entityDB entity
    (newcols', udefs, fdefs) = postgresMkColumns allDefs entity
    migrationText exists old''
        | not exists =
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

    createText newcols fdefs udspair =
        (addTable newcols entity) : uniques ++ references ++ foreignsAlt
      where
        uniques = flip concatMap udspair $ \(uname, ucols) ->
                [AlterTable name $ AddUniqueConstraint uname ucols]
        references =
            mapMaybe
                (\Column { cName, cReference } ->
                    fmap (getAddReference allDefs Nothing name cName) cReference
                )
                newcols
        foreignsAlt = map (mkForeignAlt name) fdefs

mkForeignAlt
    :: DBName
    -> ForeignDef
    -> AlterDB
mkForeignAlt name fdef =
    AlterColumn
        name
        ( foreignRefTableDBName fdef
        , addReference
        )
  where
    addReference =
        AddReference
            constraintName
            childfields
            escapedParentFields
            (foreignFieldCascade fdef)
    constraintName =
        foreignConstraintNameDBName fdef
    (childfields, parentfields) =
        unzip (map (\((_,b),(_,d)) -> (b,d)) (foreignFields fdef))
    escapedParentFields =
        map escape parentfields

addTable :: [Column] -> EntityDef -> AlterDB
addTable cols entity = AddTable $ T.concat
                       -- Lower case e: see Database.Persist.Sql.Migration
                       [ "CREATe TABLE " -- DO NOT FIX THE CAPITALIZATION!
                       , escape name
                       , "("
                       , idtxt
                       , if null cols then "" else ","
                       , T.intercalate "," $ map showColumn cols
                       , ")"
                       ]
    where
      name = entityDB entity
      idtxt = case entityPrimary entity of
                Just pdef -> T.concat [" PRIMARY KEY (", T.intercalate "," $ map (escape . fieldDB) $ compositeFields pdef, ")"]
                Nothing   ->
                    let defText = defaultAttribute $ fieldAttrs $ entityId entity
                        sType = fieldSqlType $ entityId entity
                    in  T.concat
                            [ escape $ fieldDB (entityId entity)
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
    = ChangeType SqlType Text
    | IsNull | NotNull | Add' Column | Drop SafeToRemove
    | Default Text | NoDefault | Update' Text
    | AddReference DBName [DBName] [Text] FieldCascade
    | DropReference DBName

type AlterColumn' = (DBName, AlterColumn)

data AlterTable = AddUniqueConstraint DBName [DBName]
                | DropConstraint DBName

data AlterDB = AddTable Text
             | AlterColumn DBName AlterColumn'
             | AlterTable DBName AlterTable

-- | Returns all of the columns in the given table currently in the database.
getColumns :: (Text -> IO Statement)
           -> EntityDef -> [Column]
           -> IO [Either Text (Either Column (DBName, [DBName]))]
getColumns getter def cols = do
    let sqlv=T.concat ["SELECT "
                          ,"column_name "
                          ,",is_nullable "
                          ,",COALESCE(domain_name, udt_name)" -- See DOMAINS below
                          ,",column_default "
                          ,",numeric_precision "
                          ,",numeric_scale "
                          ,",character_maximum_length "
                          ,"FROM information_schema.columns "
                          ,"WHERE table_catalog=current_database() "
                          ,"AND table_schema=current_schema() "
                          ,"AND table_name=? "
                          ,"AND column_name <> ?"]

-- DOMAINS Postgres supports the concept of domains, which are data types with optional constraints.
-- An app might make an "email" domain over the varchar type, with a CHECK that the emails are valid
-- In this case the generated SQL should use the domain name: ALTER TABLE users ALTER COLUMN foo TYPE email
-- This code exists to use the domain name (email), instead of the underlying type (varchar).
-- This is tested in EquivalentTypeTest.hs

    stmt <- getter sqlv
    let vals =
            [ PersistText $ unDBName $ entityDB def
            , PersistText $ unDBName $ fieldDB (entityId def)
            ]
    cs <- with (stmtQuery stmt vals) (\src -> runConduit $ src .| helper)
    let sqlc = T.concat ["SELECT "
                          ,"c.constraint_name, "
                          ,"c.column_name "
                          ,"FROM information_schema.key_column_usage c, "
                          ,"information_schema.table_constraints k "
                          ,"WHERE c.table_catalog=current_database() "
                          ,"AND c.table_catalog=k.table_catalog "
                          ,"AND c.table_schema=current_schema() "
                          ,"AND c.table_schema=k.table_schema "
                          ,"AND c.table_name=? "
                          ,"AND c.table_name=k.table_name "
                          ,"AND c.column_name <> ? "
                          ,"AND c.constraint_name=k.constraint_name "
                          ,"AND NOT k.constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY') "
                          ,"ORDER BY c.constraint_name, c.column_name"]

    stmt' <- getter sqlc

    us <- with (stmtQuery stmt' vals) (\src -> runConduit $ src .| helperU)
    return $ cs ++ us
  where
    refMap = Map.fromList $ foldl ref [] cols
        where ref rs c = case cReference c of
                  Nothing -> rs
                  (Just r) -> (unDBName $ cName c, r) : rs
    getAll front = do
        x <- CL.head
        case x of
            Nothing -> return $ front []
            Just [PersistText con, PersistText col] -> getAll (front . (:) (con, col))
            Just [PersistByteString con, PersistByteString col] -> getAll (front . (:) (T.decodeUtf8 con, T.decodeUtf8 col))
            Just o -> error $ "unexpected datatype returned for postgres o="++show o
    helperU = do
        rows <- getAll id
        return $ map (Right . Right . (DBName . fst . head &&& map (DBName . snd)))
               $ groupBy ((==) `on` fst) rows
    helper = do
        x <- CL.head
        case x of
            Nothing -> return []
            Just x'@((PersistText cname):_) -> do
                col <- liftIO $ getColumn getter (entityDB def) x' (Map.lookup cname refMap)
                let col' = case col of
                            Left e -> Left e
                            Right c -> Right $ Left c
                cols <- helper
                return $ col' : cols

-- | Check if a column name is listed as the "safe to remove" in the entity
-- list.
safeToRemove :: EntityDef -> DBName -> Bool
safeToRemove def (DBName colName)
    = any (elem "SafeToRemove" . fieldAttrs)
    $ filter ((== DBName colName) . fieldDB)
    $ entityFields def

getAlters :: [EntityDef]
          -> EntityDef
          -> ([Column], [(DBName, [DBName])])
          -> ([Column], [(DBName, [DBName])])
          -> ([AlterColumn'], [AlterTable])
getAlters defs def (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old = map (\x -> (cName x, Drop $ safeToRemove def $ cName x)) old
    getAltersC (new:news) old =
        let (alters, old') = findAlters defs (entityDB def) new old
         in alters ++ getAltersC news old'

    getAltersU
        :: [(DBName, [DBName])]
        -> [(DBName, [DBName])]
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
    isManual (DBName x) = "__manual_" `T.isPrefixOf` x

getColumn :: (Text -> IO Statement)
          -> DBName -> [PersistValue]
          -> Maybe (DBName, DBName)
          -> IO (Either Text Column)
getColumn getter tableName' [PersistText columnName, PersistText isNullable, PersistText typeName, defaultValue, numericPrecision, numericScale, maxlen] refName =
    case d' of
        Left s -> return $ Left s
        Right d'' ->
            let typeStr = case maxlen of
                            PersistInt64 n -> T.concat [typeName, "(", T.pack (show n), ")"]
                            _              -> typeName
             in case getType typeStr of
                  Left s -> return $ Left s
                  Right t -> do
                      let cname = DBName columnName
                      ref <- getRef cname refName
                      return $ Right Column
                          { cName = cname
                          , cNull = isNullable == "YES"
                          , cSqlType = t
                          , cDefault = fmap stripSuffixes d''
                          , cDefaultConstraintName = Nothing
                          , cMaxLen = Nothing
                          , cReference = ref
                          }
  where
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
    getRef _ Nothing = return Nothing
    getRef cname (Just (_, refName')) = do
        let sql = T.concat ["SELECT DISTINCT "
                           ,"ccu.table_name, "
                           ,"tc.constraint_name "
                           ,"FROM information_schema.constraint_column_usage ccu, "
                           ,"information_schema.key_column_usage kcu, "
                           ,"information_schema.table_constraints tc "
                           ,"WHERE tc.constraint_type='FOREIGN KEY' "
                           ,"AND kcu.constraint_name=tc.constraint_name "
                           ,"AND ccu.constraint_name=kcu.constraint_name "
                           ,"AND kcu.ordinal_position=1 "
                           ,"AND kcu.table_name=? "
                           ,"AND kcu.column_name=? "
                           ,"AND tc.constraint_name=?"]
        stmt <- getter sql
        cntrs <- with (stmtQuery stmt [PersistText $ unDBName tableName'
                                      ,PersistText $ unDBName cname
                                      ,PersistText $ unDBName refName'])
                      (\src -> runConduit $ src .| CL.consume)
        case cntrs of
          [] -> return Nothing
          [[PersistText table, PersistText constraint]] ->
            return $ Just (DBName table, DBName constraint)
          xs ->
            error $ mconcat
              [ "Postgresql.getColumn: error fetching constraints. Expected a single result for foreign key query for table: "
              , T.unpack (unDBName tableName')
              , " and column: "
              , T.unpack (unDBName cname)
              , " but got: "
              , show xs
              ]
    d' = case defaultValue of
            PersistNull   -> Right Nothing
            PersistText t -> Right $ Just t
            _ -> Left $ T.pack $ "Invalid default column: " ++ show defaultValue
    getType "int4"        = Right SqlInt32
    getType "int8"        = Right SqlInt64
    getType "varchar"     = Right SqlString
    getType "text"        = Right SqlString
    getType "date"        = Right SqlDay
    getType "bool"        = Right SqlBool
    getType "timestamptz" = Right SqlDayTime
    getType "float4"      = Right SqlReal
    getType "float8"      = Right SqlReal
    getType "bytea"       = Right SqlBlob
    getType "time"        = Right SqlTime
    getType "numeric"     = getNumeric numericPrecision numericScale
    getType a             = Right $ SqlOther a

    getNumeric (PersistInt64 a) (PersistInt64 b) = Right $ SqlNumeric (fromIntegral a) (fromIntegral b)
    getNumeric PersistNull PersistNull = Left $ T.concat
      [ "No precision and scale were specified for the column: "
      , columnName
      , " in table: "
      , unDBName tableName'
      , ". Postgres defaults to a maximum scale of 147,455 and precision of 16383,"
      , " which is probably not what you intended."
      , " Specify the values as numeric(total_digits, digits_after_decimal_place)."
      ]
    getNumeric a b = Left $ T.concat
      [ "Can not get numeric field precision for the column: "
      , columnName
      , " in table: "
      , unDBName tableName'
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

findAlters :: [EntityDef] -> DBName -> Column -> [Column] -> ([AlterColumn'], [Column])
findAlters defs _tablename col@(Column name isNull sqltype def _defConstraintName _maxLen ref) cols =
    case filter (\c -> cName c == name) cols of
        [] -> ([(name, Add' col)], cols)
        Column _ isNull' sqltype' def' _defConstraintName' _maxLen' ref':_ ->
            let refDrop Nothing = []
                refDrop (Just (_, cname)) = [(name, DropReference cname)]
                refAdd Nothing = []
                refAdd (Just (tname, a)) =
                    case find ((==tname) . entityDB) defs of
                        Just refdef ->
                            [ ( tname
                              , AddReference
                                    a
                                    [name]
                                    (Util.dbIdColumnsEsc escape refdef)
                                    noCascade
                              )
                            ]
                        Nothing ->
                            error $ "could not find the entityDef for reftable[" ++ show tname ++ "]"
                modRef =
                    if fmap snd ref == fmap snd ref'
                        then []
                        else refDrop ref' ++ refAdd ref
                modNull = case (isNull, isNull') of
                            (True, False) -> [(name, IsNull)]
                            (False, True) ->
                                let up = case def of
                                            Nothing -> id
                                            Just s -> (:) (name, Update' s)
                                 in up [(name, NotNull)]
                            _ -> []
                modType
                    | sqlTypeEq sqltype sqltype' = []
                    -- When converting from Persistent pre-2.0 databases, we
                    -- need to make sure that TIMESTAMP WITHOUT TIME ZONE is
                    -- treated as UTC.
                    | sqltype == SqlDayTime && sqltype' == SqlOther "timestamp" =
                        [(name, ChangeType sqltype $ T.concat
                            [ " USING "
                            , escape name
                            , " AT TIME ZONE 'UTC'"
                            ])]
                    | otherwise = [(name, ChangeType sqltype "")]
                modDef =
                    if def == def'
                        then []
                        else case def of
                                Nothing -> [(name, NoDefault)]
                                Just s -> [(name, Default s)]
             in (modRef ++ modDef ++ modNull ++ modType,
                 filter (\c -> cName c /= name) cols)

-- | Get the references to be added to a table for the given column.
getAddReference
    :: [EntityDef]
    -> Maybe ForeignDef
    -> DBName
    -> DBName
    -> (DBName, DBName)
    -> AlterDB
getAddReference allDefs mforeignDef table cname (s, constraintName) =
    AlterColumn
        table
        ( s
        , AddReference constraintName [cname] id_ (maybe noCascade foreignFieldCascade mforeignDef)
        )
  where
    id_ =
        fromMaybe
            (error $ "Could not find ID of entity " ++ show s)
            $ do
                entDef <- find ((== s) . entityDB) allDefs
                return $ Util.dbIdColumnsEsc escape entDef


showColumn :: Column -> Text
showColumn (Column n nu sqlType' def _defConstraintName _maxLen _ref) = T.concat
    [ escape n
    , " "
    , showSqlType sqlType'
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s -> " DEFAULT " <> s
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
showAlterDb (AlterColumn t (c, ac)) =
    (isUnsafe ac, showAlter t (c, ac))
  where
    isUnsafe (Drop safeRemove) = not safeRemove
    isUnsafe _ = False
showAlterDb (AlterTable t at) = (False, showAlterTable t at)

showAlterTable :: DBName -> AlterTable -> Text
showAlterTable table (AddUniqueConstraint cname cols) = T.concat
    [ "ALTER TABLE "
    , escape table
    , " ADD CONSTRAINT "
    , escape cname
    , " UNIQUE("
    , T.intercalate "," $ map escape cols
    , ")"
    ]
showAlterTable table (DropConstraint cname) = T.concat
    [ "ALTER TABLE "
    , escape table
    , " DROP CONSTRAINT "
    , escape cname
    ]

showAlter :: DBName -> AlterColumn' -> Text
showAlter table (n, ChangeType t extra) =
    T.concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " TYPE "
        , showSqlType t
        , extra
        ]
showAlter table (n, IsNull) =
    T.concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " DROP NOT NULL"
        ]
showAlter table (n, NotNull) =
    T.concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " SET NOT NULL"
        ]
showAlter table (_, Add' col) =
    T.concat
        [ "ALTER TABLE "
        , escape table
        , " ADD COLUMN "
        , showColumn col
        ]
showAlter table (n, Drop _) =
    T.concat
        [ "ALTER TABLE "
        , escape table
        , " DROP COLUMN "
        , escape n
        ]
showAlter table (n, Default s) =
    T.concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " SET DEFAULT "
        , s
        ]
showAlter table (n, NoDefault) = T.concat
    [ "ALTER TABLE "
    , escape table
    , " ALTER COLUMN "
    , escape n
    , " DROP DEFAULT"
    ]
showAlter table (n, Update' s) = T.concat
    [ "UPDATE "
    , escape table
    , " SET "
    , escape n
    , "="
    , s
    , " WHERE "
    , escape n
    , " IS NULL"
    ]
showAlter table (reftable, AddReference fkeyname t2 id2 cascade) = T.concat
    [ "ALTER TABLE "
    , escape table
    , " ADD CONSTRAINT "
    , escape fkeyname
    , " FOREIGN KEY("
    , T.intercalate "," $ map escape t2
    , ") REFERENCES "
    , escape reftable
    , "("
    , T.intercalate "," id2
    , ")"
    ] <> renderFieldCascade cascade
showAlter table (_, DropReference cname) = T.concat
    [ "ALTER TABLE "
    , escape table
    , " DROP CONSTRAINT "
    , escape cname
    ]

-- | Get the SQL string for the table that a PeristEntity represents.
-- Useful for raw SQL queries.
tableName :: (PersistEntity record) => record -> Text
tableName = escape . tableDBName

-- | Get the SQL string for the field that an EntityField represents.
-- Useful for raw SQL queries.
fieldName :: (PersistEntity record) => EntityField record typ -> Text
fieldName = escape . fieldDBName

escape :: DBName -> Text
escape (DBName s) =
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
    , pgPoolSize :: Int
      -- ^ How many connections should be held in the connection pool.
    } deriving (Show, Read, Data, Typeable)

instance FromJSON PostgresConf where
    parseJSON v = modifyFailure ("Persistent: error loading PostgreSQL conf: " ++) $
      flip (withObject "PostgresConf") v $ \o -> do
        database <- o .: "database"
        host     <- o .: "host"
        port     <- o .:? "port" .!= 5432
        user     <- o .: "user"
        password <- o .: "password"
        pool     <- o .: "poolsize"
        let ci = PG.ConnectInfo
                   { PG.connectHost     = host
                   , PG.connectPort     = port
                   , PG.connectUser     = user
                   , PG.connectPassword = password
                   , PG.connectDatabase = database
                   }
            cstr = PG.postgreSQLConnectionString ci
        return $ PostgresConf cstr pool
instance PersistConfig PostgresConf where
    type PersistConfigBackend PostgresConf = SqlPersistT
    type PersistConfigPool PostgresConf = ConnectionPool
    createPoolConfig (PostgresConf cs size) = runNoLoggingT $ createPostgresqlPool cs size -- FIXME
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

refName :: DBName -> DBName -> DBName
refName (DBName table) (DBName column) =
    let overhead = T.length $ T.concat ["_", "_fkey"]
        (fromTable, fromColumn) = shortenNames overhead (T.length table, T.length column)
    in DBName $ T.concat [T.take fromTable table, "_", T.take fromColumn column, "_fkey"]

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

udToPair :: UniqueDef -> (DBName, [DBName])
udToPair ud = (uniqueDBName ud, map snd $ uniqueFields ud)

mockMigrate :: [EntityDef]
         -> (Text -> IO Statement)
         -> EntityDef
         -> IO (Either [Text] [(Bool, Text)])
mockMigrate allDefs _ entity = fmap (fmap $ map showAlterDb) $ do
    case partitionEithers [] of
        ([], old'') -> return $ Right $ migrationText False old''
        (errs, _) -> return $ Left errs
  where
    name = entityDB entity
    migrationText exists old'' =
        if not exists
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
                    fmap (getAddReference allDefs Nothing name cName) cReference
                )
                $ newcols
        foreignsAlt = map (mkForeignAlt name) fdefs

-- | Mock a migration even when the database is not present.
-- This function performs the same functionality of 'printMigration'
-- with the difference that an actual database is not needed.
mockMigration :: Migration -> IO ()
mockMigration mig = do
  smap <- newIORef $ Map.empty
  let sqlbackend = SqlBackend { connPrepare = \_ -> do
                                             return Statement
                                                        { stmtFinalize = return ()
                                                        , stmtReset = return ()
                                                        , stmtExecute = undefined
                                                        , stmtQuery = \_ -> return $ return ()
                                                        },
                             connInsertManySql = Nothing,
                             connInsertSql = undefined,
                             connUpsertSql = Nothing,
                             connPutManySql = Nothing,
                             connStmtMap = smap,
                             connClose = undefined,
                             connMigrateSql = mockMigrate,
                             connBegin = undefined,
                             connCommit = undefined,
                             connRollback = undefined,
                             connEscapeName = escape,
                             connNoLimit = undefined,
                             connRDBMS = undefined,
                             connLimitOffset = undefined,
                             connLogFunc = undefined,
                             connMaxParams = Nothing,
                             connRepsertManySql = Nothing
                             }
      result = runReaderT $ runWriterT $ runWriterT mig
  resp <- result sqlbackend
  mapM_ T.putStrLn $ map snd $ snd resp

putManySql :: EntityDef -> Int -> Text
putManySql ent n = putManySql' conflictColumns fields ent n
  where
    fields = entityFields ent
    conflictColumns = concatMap (map (escape . snd) . uniqueFields) (entityUniques ent)

repsertManySql :: EntityDef -> Int -> Text
repsertManySql ent n = putManySql' conflictColumns fields ent n
  where
    fields = keyAndEntityFields ent
    conflictColumns = escape . fieldDB <$> entityKeyFields ent

putManySql' :: [Text] -> [FieldDef] -> EntityDef -> Int -> Text
putManySql' conflictColumns fields ent n = q
  where
    fieldDbToText = escape . fieldDB
    mkAssignment f = T.concat [f, "=EXCLUDED.", f]

    table = escape . entityDB $ ent
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
postgresMkColumns allDefs t = mkColumns allDefs t (emptyBackendSpecificOverrides {backendSpecificForeignKeyName = Just refName})

