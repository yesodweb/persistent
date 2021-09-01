module Database.Persist.Sql.Types
    ( module Database.Persist.Sql.Types
    , SqlBackend, SqlReadBackend (..), SqlWriteBackend (..)
    , Statement (..), LogFunc, InsertSqlResult (..)
    , readToUnknown, readToWrite, writeToUnknown
    , SqlBackendCanRead, SqlBackendCanWrite, SqlReadT, SqlWriteT, IsSqlBackend
    , OverflowNatural(..)
    , ConnectionPoolConfig(..)
    ) where

import Control.Exception (Exception(..))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Pool (Pool)
import Data.Text (Text, unpack)

import Data.Time (NominalDiffTime)
import Database.Persist.Sql.Types.Internal
import Database.Persist.Types

data Column = Column
    { cName      :: !FieldNameDB
    , cNull      :: !Bool
    , cSqlType   :: !SqlType
    , cDefault   :: !(Maybe Text)
    , cGenerated :: !(Maybe Text)
    , cDefaultConstraintName   :: !(Maybe ConstraintNameDB)
    , cMaxLen    :: !(Maybe Integer)
    , cReference :: !(Maybe ColumnReference)
    }
    deriving (Eq, Ord, Show)

-- | This value specifies how a field references another table.
--
-- @since 2.11.0.0
data ColumnReference = ColumnReference
    { crTableName :: !EntityNameDB
    -- ^ The table name that the
    --
    -- @since 2.11.0.0
    , crConstraintName :: !ConstraintNameDB
    -- ^ The name of the foreign key constraint.
    --
    -- @since 2.11.0.0
    , crFieldCascade :: !FieldCascade
    -- ^ Whether or not updates/deletions to the referenced table cascade
    -- to this table.
    --
    -- @since 2.11.0.0
    }
    deriving (Eq, Ord, Show)

data PersistentSqlException = StatementAlreadyFinalized Text
                            | Couldn'tGetSQLConnection
    deriving Show
instance Exception PersistentSqlException

type SqlPersistT = ReaderT SqlBackend

type SqlPersistM = SqlPersistT (NoLoggingT (ResourceT IO))

type ConnectionPool = Pool SqlBackend

-- | Values to configure a pool of database connections. See "Data.Pool" for details.
--
-- @since 2.11.0.0
data ConnectionPoolConfig = ConnectionPoolConfig
    { connectionPoolConfigStripes :: Int -- ^ How many stripes to divide the pool into. See "Data.Pool" for details. Default: 1.
    , connectionPoolConfigIdleTimeout :: NominalDiffTime -- ^ How long connections can remain idle before being disposed of, in seconds. Default: 600
    , connectionPoolConfigSize :: Int -- ^ How many connections should be held in the connection pool. Default: 10
    }
    deriving (Show)

-- TODO: Bad defaults for SQLite maybe?
-- | Initializes a ConnectionPoolConfig with default values. See the documentation of 'ConnectionPoolConfig' for each field's default value.
--
-- @since 2.11.0.0
defaultConnectionPoolConfig :: ConnectionPoolConfig
defaultConnectionPoolConfig = ConnectionPoolConfig 1 600 10

-- $rawSql
--
-- Although it covers most of the useful cases, @persistent@'s
-- API may not be enough for some of your tasks.  May be you need
-- some complex @JOIN@ query, or a database-specific command
-- needs to be issued.
--
-- To issue raw SQL queries, use 'rawSql'. It does all the hard work of
-- automatically parsing the rows of the result.  It may return:
--
--   * An 'Entity', that which 'selectList' returns.
--     All of your entity's fields are
--     automatically parsed.
--
--   * A @'Single' a@, which is a single, raw column of type @a@.
--     You may use a Haskell type (such as in your entity
--     definitions), for example @Single Text@ or @Single Int@,
--     or you may get the raw column value with @Single
--     'PersistValue'@.
--
--   * A tuple combining any of these (including other tuples).
--     Using tuples allows you to return many entities in one
--     query.
--
-- The only difference between issuing SQL queries with 'rawSql'
-- and using other means is that we have an /entity selection/
-- /placeholder/, the double question mark @??@.  It /must/ be
-- used whenever you want to @SELECT@ an 'Entity' from your
-- query.  Here's a sample SQL query @sampleStmt@ that may be
-- issued:
--
-- @
-- SELECT ??, ??
-- FROM \"Person\", \"Likes\", \"Object\"
-- WHERE \"Person\".id = \"Likes\".\"personId\"
-- AND \"Object\".id = \"Likes\".\"objectId\"
-- AND \"Person\".name LIKE ?
-- @
--
-- To use that query, you could say
--
-- @
-- do results <- 'rawSql' sampleStmt [\"%Luke%\"]
--    forM_ results $
--      \\( Entity personKey person
--       , Entity objectKey object
--       ) -> do ...
-- @
--
-- Note that 'rawSql' knows how to replace the double question
-- marks @??@ because of the type of the @results@.


-- | A single column (see 'rawSql').  Any 'PersistField' may be
-- used here, including 'PersistValue' (which does not do any
-- processing).
newtype Single a = Single {unSingle :: a}
    deriving (Eq, Ord, Show, Read)

