{-# LANGUAGE RankNTypes #-}

module Database.Persist.SqlBackend.Internal.MkSqlBackend where

import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import Data.IORef
import Data.Map (Map)
import Data.Text (Text)
import Database.Persist.SqlBackend.Internal.Statement
import Database.Persist.SqlBackend.Internal.InsertSqlResult
import Database.Persist.SqlBackend.Internal.IsolationLevel
import Database.Persist.Types.Base
import Database.Persist.Names

-- | This type shares many of the same field names as the 'SqlBackend' type.
-- It's useful for library authors to use this when migrating from using the
-- 'SqlBackend' constructor directly to the 'mkSqlBackend' function.
--
-- This type will only contain required fields for constructing a 'SqlBackend'.
-- For fields that aren't present on this record, you'll want to use the various
-- @set@ functions or
--
-- @since 2.13.0.0
data MkSqlBackendArgs = MkSqlBackendArgs
    { connPrepare :: Text -> IO Statement
    -- ^ This function should prepare a 'Statement' in the target database,
    -- which should allow for efficient query reuse.
    , connInsertSql :: EntityDef -> [PersistValue] -> InsertSqlResult
    -- ^ This function generates the SQL and values necessary for
    -- performing an insert against the database.
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
    }

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
