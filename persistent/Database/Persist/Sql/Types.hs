module Database.Persist.Sql.Types
    ( module Database.Persist.Sql.Types
    , SqlBackend (..), SqlReadBackend (..), SqlWriteBackend (..)
    , Statement (..), LogFunc, InsertSqlResult (..)
    , readToUnknown, readToWrite, writeToUnknown
    , SqlBackendCanRead, SqlBackendCanWrite, SqlReadT, SqlWriteT, IsSqlBackend
    , OverflowNatural(..)
    ) where

import Control.Exception (Exception(..))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Pool (Pool)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)

import Database.Persist.Types
import Database.Persist.Sql.Types.Internal

data Column = Column
    { cName      :: !DBName
    , cNull      :: !Bool
    , cSqlType   :: !SqlType
    , cDefault   :: !(Maybe Text)
    , cDefaultConstraintName   :: !(Maybe DBName)
    , cMaxLen    :: !(Maybe Integer)
    , cReference :: !(Maybe (DBName, DBName)) -- table name, constraint name
    }
    deriving (Eq, Ord, Show)

data PersistentSqlException = StatementAlreadyFinalized Text
                            | Couldn'tGetSQLConnection
    deriving (Typeable, Show)
instance Exception PersistentSqlException

type SqlPersistT = ReaderT SqlBackend

type SqlPersistM = SqlPersistT (NoLoggingT (ResourceT IO))

type Sql = Text

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]

-- | A 'Migration' is a four level monad stack consisting of:
--
-- * @'WriterT' ['Text']@ representing a log of errors in the migrations.
-- * @'WriterT' 'CautiousMigration'@ representing a list of migrations to
--   run, along with whether or not they are safe.
-- * @'ReaderT' 'SqlBackend'@, aka the 'SqlPersistT' transformer for
--   database interop.
-- * @'IO'@ for arbitrary IO.
type Migration = WriterT [Text] (WriterT CautiousMigration (ReaderT SqlBackend IO)) ()

type ConnectionPool = Pool SqlBackend

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

-- | An exception indicating that Persistent refused to run some unsafe
-- migrations. Contains a list of pairs where the Bool tracks whether the
-- migration was unsafe (True means unsafe), and the Sql is the sql statement
-- for the migration.
--
-- @since 2.11.1.0
newtype PersistUnsafeMigrationException
  = PersistUnsafeMigrationException [(Bool, Sql)]
  deriving Typeable

-- | This 'Show' instance renders an error message suitable for printing to the
-- console. This is a little dodgy, but since GHC uses Show instances when
-- displaying uncaught exceptions, we have little choice.
instance Show PersistUnsafeMigrationException where
  show (PersistUnsafeMigrationException mig) =
    concat
      [ "\n\nDatabase migration: manual intervention required.\n"
      , "The unsafe actions are prefixed by '***' below:\n\n"
      , unlines $ map displayMigration mig
      ]
    where
      displayMigration :: (Bool, Sql) -> String
      displayMigration (True,  s) = "*** " ++ unpack s ++ ";"
      displayMigration (False, s) = "    " ++ unpack s ++ ";"

instance Exception PersistUnsafeMigrationException
