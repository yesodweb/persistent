{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Persist.Sql.Types where

import Control.Exception (Exception)
import Control.Monad.Trans.Resource (MonadResource (..), ResourceT)
import Data.Acquire (Acquire)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer (WriterT)
import Data.Typeable (Typeable)
import Control.Monad (liftM)
import Database.Persist.Types
import Database.Persist.Class (HasPersistBackend (..))
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Int (Int64)
import Data.Conduit (Source)
import Data.Pool (Pool)
import Language.Haskell.TH.Syntax (Loc)
import Control.Monad.Logger (LogSource, LogLevel)
import System.Log.FastLogger (LogStr)
import Data.Text (Text)

data InsertSqlResult = ISRSingle Text
                     | ISRInsertGet Text Text
                     | ISRManyKeys Text [PersistValue]

-- | Deprecated synonym for @SqlBackend@.
type Connection = SqlBackend
{-# DEPRECATED Connection "Please use SqlBackend instead" #-}

data SqlBackend = SqlBackend
    { connPrepare :: Text -> IO Statement
    -- | table name, column names, id name, either 1 or 2 statements to run
    , connInsertSql :: EntityDef -> [PersistValue] -> InsertSqlResult
    , connInsertManySql :: EntityDef -> [[PersistValue]] -> InsertSqlResult
    , connStmtMap :: IORef (Map Text Statement)
    , connClose :: IO ()
    , connMigrateSql
        :: [EntityDef]
        -> (Text -> IO Statement)
        -> EntityDef
        -> IO (Either [Text] [(Bool, Text)])
    , connBegin :: (Text -> IO Statement) -> IO ()
    , connCommit :: (Text -> IO Statement) -> IO ()
    , connRollback :: (Text -> IO Statement) -> IO ()
    , connEscapeName :: DBName -> Text
    , connNoLimit :: Text
    , connRDBMS :: Text
    , connLimitOffset :: (Int,Int) -> Bool -> Text -> Text
    , connLogFunc :: LogFunc
    }
    deriving Typeable
instance HasPersistBackend SqlBackend SqlBackend where
    persistBackend = id

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data Statement = Statement
    { stmtFinalize :: IO ()
    , stmtReset :: IO ()
    , stmtExecute :: [PersistValue] -> IO Int64
    , stmtQuery :: forall m. MonadIO m
                => [PersistValue]
                -> Acquire (Source m [PersistValue])
    }

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

type SqlPersist = SqlPersistT
{-# DEPRECATED SqlPersist "Please use SqlPersistT instead" #-}

type SqlPersistM = SqlPersistT (NoLoggingT (ResourceT IO))

type Sql = Text

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]

type Migration = WriterT [Text] (WriterT CautiousMigration (ReaderT SqlBackend IO)) ()

type ConnectionPool = Pool SqlBackend

-- $rawSql
--
-- Although it covers most of the useful cases, @persistent@'s
-- API may not be enough for some of your tasks.  May be you need
-- some complex @JOIN@ query, or a database-specific command
-- needs to be issued.
--
-- To issue raw SQL queries you could use 'R.withStmt', which
-- allows you to do anything you need.  However, its API is
-- /low-level/ and you need to parse each row yourself.  However,
-- most of your complex queries will have simple results -- some
-- of your entities and maybe a couple of derived columns.
--
-- This is where 'rawSql' comes in.  Like 'R.withStmt', you may
-- issue /any/ SQL query.  However, it does all the hard work for
-- you and automatically parses the rows of the result.  It may
-- return:
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
