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
import Control.Monad.Trans.Resource (MonadResource (..), MonadThrow (..), ResourceT, Resource)
import Control.Monad.Logger (MonadLogger (..), NoLoggingT)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Applicative (Applicative (..))
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Base (MonadBase (..))
import Control.Monad (MonadPlus (..))
import Data.Typeable (Typeable)
import Control.Monad (liftM)
import Database.Persist.Types
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Int (Int64)
import Data.Conduit (Source)
import Data.Conduit.Pool (Pool)
import Web.PathPieces
import Control.Exception (throw)
import qualified Data.Text.Read
import Language.Haskell.TH.Syntax (Loc)
import Control.Monad.Logger (LogSource, LogLevel)
import System.Log.FastLogger (LogStr)

data InsertSqlResult = ISRSingle Text
                     | ISRInsertGet Text Text
                     | ISRManyKeys Text [PersistValue]

data Connection = Connection
    { connPrepare :: Text -> IO Statement
    -- | table name, column names, id name, either 1 or 2 statements to run
    , connInsertSql :: EntityDef SqlType -> [PersistValue] -> InsertSqlResult
    , connStmtMap :: IORef (Map Text Statement)
    , connClose :: IO ()
    , connMigrateSql
        :: [EntityDef SqlType]
        -> (Text -> IO Statement)
        -> EntityDef SqlType
        -> IO (Either [Text] [(Bool, Text)])
    , connBegin :: (Text -> IO Statement) -> IO ()
    , connCommit :: (Text -> IO Statement) -> IO ()
    , connRollback :: (Text -> IO Statement) -> IO ()
    , connEscapeName :: DBName -> Text
    , connNoLimit :: Text
    , connRDBMS :: Text
    , connLimitOffset :: (Int,Int) -> Bool -> Text -> Text
    , connLogFunc :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    }
    deriving Typeable

data Statement = Statement
    { stmtFinalize :: IO ()
    , stmtReset :: IO ()
    , stmtExecute :: [PersistValue] -> IO Int64
    , stmtQuery :: forall m. MonadIO m
                => [PersistValue]
                -> Resource (Source m [PersistValue])
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

type SqlBackend = Connection -- FIXME

newtype SqlPersistT m a = SqlPersistT { unSqlPersistT :: ReaderT Connection m a } -- switch to type synonym, rename Connection
    deriving (Monad, MonadIO, MonadTrans, Functor, Applicative, MonadPlus)

type SqlPersist = SqlPersistT
{-# DEPRECATED SqlPersist "Please use SqlPersistT instead" #-}

type SqlPersistM = SqlPersistT (NoLoggingT (ResourceT IO))

instance MonadThrow m => MonadThrow (SqlPersistT m) where
    monadThrow = lift . monadThrow

instance MonadBase backend m => MonadBase backend (SqlPersistT m) where
    liftBase = lift . liftBase

instance MonadBaseControl backend m => MonadBaseControl backend (SqlPersistT m) where
     newtype StM (SqlPersistT m) a = StMSP {unStMSP :: ComposeSt SqlPersistT m a}
     liftBaseWith = defaultLiftBaseWith StMSP
     restoreM     = defaultRestoreM   unStMSP
instance MonadTransControl SqlPersistT where
    newtype StT SqlPersistT a = StReader {unStReader :: a}
    liftWith f = SqlPersistT $ ReaderT $ \r -> f $ \t -> liftM StReader $ runReaderT (unSqlPersistT t) r
    restoreT = SqlPersistT . ReaderT . const . liftM unStReader

instance MonadResource m => MonadResource (SqlPersistT m) where
    liftResourceT = lift . liftResourceT

instance MonadLogger m => MonadLogger (SqlPersistT m) where
    monadLoggerLog a b c = lift . monadLoggerLog a b c

type Sql = Text

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]

type Migration m = WriterT [Text] (WriterT CautiousMigration (ReaderT Connection m)) ()

type ConnectionPool = Pool Connection

instance PathPiece (KeyBackend SqlBackend entity) where
    toPathPiece (Key (PersistInt64 i)) = toPathPiece i
    toPathPiece k = throw $ PersistInvalidField $ pack $ "Invalid Key: " ++ show k
    fromPathPiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, t') | T.null t' -> Just $ Key $ PersistInt64 i
            _ -> Nothing


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
