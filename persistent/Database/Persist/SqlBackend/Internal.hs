{-# language RecordWildCards #-}
{-# language RankNTypes #-}

module Database.Persist.SqlBackend.Internal where

import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Database.Persist.Class.PersistStore
import Database.Persist.Types.Base
import Database.Persist.Names
import Data.IORef
import Database.Persist.SqlBackend.Internal.MkSqlBackend
import Database.Persist.SqlBackend.Internal.Statement
import Database.Persist.SqlBackend.Internal.InsertSqlResult
import Database.Persist.SqlBackend.Internal.IsolationLevel

-- | A 'SqlBackend' represents a handle or connection to a database. It
-- contains functions and values that allow databases to have more
-- optimized implementations, as well as references that benefit
-- performance and sharing.
--
-- Instead of using the 'SqlBackend' constructor directly, use the
-- 'mkSqlBackend' function.
--
-- A 'SqlBackend' is *not* thread-safe. You should not assume that
-- a 'SqlBackend' can be shared among threads and run concurrent queries.
-- This *will* result in problems. Instead, you should create a @'Pool'
-- 'SqlBackend'@, known as a 'ConnectionPool', and pass that around in
-- multi-threaded applications.
--
-- To run actions in the @persistent@ library, you should use the
-- 'runSqlConn' function. If you're using a multithreaded application, use
-- the 'runSqlPool' function.
data SqlBackend = SqlBackend
    { connPrepare :: Text -> IO Statement
    -- ^ This function should prepare a 'Statement' in the target database,
    -- which should allow for efficient query reuse.
    , connInsertSql :: EntityDef -> [PersistValue] -> InsertSqlResult
    -- ^ This function generates the SQL and values necessary for
    -- performing an insert against the database.
    , connInsertManySql :: Maybe (EntityDef -> [[PersistValue]] -> InsertSqlResult)
    -- ^ SQL for inserting many rows and returning their primary keys, for
    -- backends that support this functionality. If 'Nothing', rows will be
    -- inserted one-at-a-time using 'connInsertSql'.
    , connUpsertSql :: Maybe (EntityDef -> NonEmpty (FieldNameHS, FieldNameDB) -> Text -> Text)
    -- ^ Some databases support performing UPSERT _and_ RETURN entity
    -- in a single call.
    --
    -- This field when set will be used to generate the UPSERT+RETURN sql given
    -- * an entity definition
    -- * updates to be run on unique key(s) collision
    --
    -- When left as 'Nothing', we find the unique key from entity def before
    -- * trying to fetch an entity by said key
    -- * perform an update when result found, else issue an insert
    -- * return new entity from db
    --
    -- @since 2.6
    , connPutManySql :: Maybe (EntityDef -> Int -> Text)
    -- ^ Some databases support performing bulk UPSERT, specifically
    -- "insert or replace many records" in a single call.
    --
    -- This field when set, given
    -- * an entity definition
    -- * number of records to be inserted
    -- should produce a PUT MANY sql with placeholders for records
    --
    -- When left as 'Nothing', we default to using 'defaultPutMany'.
    --
    -- @since 2.8.1
    , connStmtMap :: IORef (Map Text Statement)
    -- ^ A reference to the cache of statements. 'Statement's are keyed by
    -- the 'Text' queries that generated them.
    , connClose :: IO ()
    -- ^ Close the underlying connection.
    , connMigrateSql
        :: [EntityDef]
        -> (Text -> IO Statement)
        -> EntityDef
        -> IO (Either [Text] [(Bool, Text)])
    -- ^ This function returns the migrations required to include the
    -- 'EntityDef' parameter in the @['EntityDef']@ database. This might
    -- include creating a new table if the entity is not present, or
    -- altering an existing table if it is.
    , connBegin :: (Text -> IO Statement) -> Maybe IsolationLevel -> IO ()
    -- ^ A function to begin a transaction for the underlying database.
    , connCommit :: (Text -> IO Statement) -> IO ()
    -- ^ A function to commit a transaction to the underlying database.
    , connRollback :: (Text -> IO Statement) -> IO ()
    -- ^ A function to roll back a transaction on the underlying database.
    , connEscapeFieldName :: FieldNameDB -> Text
    -- ^ A function to extract and escape the name of the column corresponding
    -- to the provided field.
    --
    -- @since 2.12.0.0
    , connEscapeTableName :: EntityDef -> Text
    -- ^ A function to extract and escape the name of the table corresponding
    -- to the provided entity. PostgreSQL uses this to support schemas.
    --
    -- @since 2.12.0.0
    , connEscapeRawName :: Text -> Text
    -- ^ A function to escape raw DB identifiers. MySQL uses backticks, while
    -- PostgreSQL uses quotes, and so on.
    --
    -- @since 2.12.0.0
    , connNoLimit :: Text
    , connRDBMS :: Text
    -- ^ A tag displaying what database the 'SqlBackend' is for. Can be
    -- used to differentiate features in downstream libraries for different
    -- database backends.
    , connLimitOffset :: (Int,Int) -> Text -> Text
    -- ^ Attach a 'LIMIT/OFFSET' clause to a SQL query. Note that
    -- LIMIT/OFFSET is problematic for performance, and indexed range
    -- queries are the superior way to offer pagination.
    , connLogFunc :: LogFunc
    -- ^ A log function for the 'SqlBackend' to use.
    , connMaxParams :: Maybe Int
    -- ^ Some databases (probably only Sqlite) have a limit on how
    -- many question-mark parameters may be used in a statement
    --
    -- @since 2.6.1
    , connRepsertManySql :: Maybe (EntityDef -> Int -> Text)
    -- ^ Some databases support performing bulk an atomic+bulk INSERT where
    -- constraint conflicting entities can replace existing entities.
    --
    -- This field when set, given
    -- * an entity definition
    -- * number of records to be inserted
    -- should produce a INSERT sql with placeholders for primary+record fields
    --
    -- When left as 'Nothing', we default to using 'defaultRepsertMany'.
    --
    -- @since 2.9.0
    }

-- | A function for creating a value of the 'SqlBackend' type. You should prefer
-- to use this instead of the constructor for 'SqlBackend', because default
-- values for this will be provided for new fields on the record when new
-- functionality is added.
--
-- @since 2.13.0.0
mkSqlBackend :: MkSqlBackendArgs -> SqlBackend
mkSqlBackend MkSqlBackendArgs {..} =
    SqlBackend
        { connMaxParams = Nothing
        , connRepsertManySql = Nothing
        , connPutManySql = Nothing
        , connUpsertSql = Nothing
        , connInsertManySql = Nothing
        , ..
        }

instance HasPersistBackend SqlBackend where
    type BaseBackend SqlBackend = SqlBackend
    persistBackend = id

instance IsPersistBackend SqlBackend where
    mkPersistBackend = id
