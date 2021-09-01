-- | This module is the primary entry point if you're working with @persistent@
-- on a SQL database.
--
-- = Getting Started
--
-- First, you'll want to define your database entities. You can do that with
-- "Database.Persist.Quasi."
--
-- Then, you'll use the operations
module Database.Persist.Sql
    (
    -- * 'RawSql' and 'PersistFieldSql'
      module Database.Persist.Sql.Class
    -- * Running actions
    -- | Run actions in a transaction with 'runSqlPool'.
    , module Database.Persist.Sql.Run
    -- * Migrations
    , module Database.Persist.Sql.Migration
    -- * @persistent@ combinators
    -- | We re-export "Database.Persist" here, to make it easier to use query
    -- and update combinators. Check out that module for documentation.
    , module Database.Persist
    , module Database.Persist.Sql.Orphan.PersistStore
    -- * The Escape Hatch
    -- | @persistent@ offers a set of functions that are useful for operating
    -- directly on the underlying SQL database. This can allow you to use
    -- whatever SQL features you want.
    --
    -- Consider going to <https://hackage.haskell.org/package/esqueleto
    -- esqueleto> for a more powerful SQL query library built on @persistent@.
    , rawQuery
    , rawQueryRes
    , rawExecute
    , rawExecuteCount
    , rawSql
    -- * SQL helpers
    , deleteWhereCount
    , updateWhereCount
    , filterClause
    , filterClauseWithVals
    , FilterTablePrefix (..)
    -- * Transactions
    , transactionSave
    , transactionSaveWithIsolation
    , transactionUndo
    , transactionUndoWithIsolation
    -- * Other utilities
    , getStmtConn
    , mkColumns
    , BackendSpecificOverrides
    , emptyBackendSpecificOverrides
    , getBackendSpecificForeignKeyName
    , setBackendSpecificForeignKeyName
    , defaultAttribute
      -- * Internal
    , IsolationLevel(..)
    , decorateSQLWithLimitOffset
    , module Database.Persist.Sql.Types
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask)

import Database.Persist
import Database.Persist.Sql.Class
import Database.Persist.Sql.Internal
import Database.Persist.Sql.Migration
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Run hiding (rawAcquireSqlConn, rawRunSqlPool)
import Database.Persist.Sql.Types
import Database.Persist.Sql.Types.Internal (IsolationLevel(..), SqlBackend(..))

import Database.Persist.Sql.Orphan.PersistQuery
import Database.Persist.Sql.Orphan.PersistStore
import Database.Persist.Sql.Orphan.PersistUnique ()

-- | Commit the current transaction and begin a new one.
-- This is used when a transaction commit is required within the context of 'runSqlConn'
-- (which brackets its provided action with a transaction begin/commit pair).
--
-- @since 1.2.0
transactionSave :: MonadIO m => ReaderT SqlBackend m ()
transactionSave = do
    conn <- ask
    let getter = getStmtConn conn
    liftIO $ connCommit conn getter >> connBegin conn getter Nothing

-- | Commit the current transaction and begin a new one with the specified isolation level.
--
-- @since 2.9.0
transactionSaveWithIsolation :: MonadIO m => IsolationLevel -> ReaderT SqlBackend m ()
transactionSaveWithIsolation isolation = do
    conn <- ask
    let getter = getStmtConn conn
    liftIO $ connCommit conn getter >> connBegin conn getter (Just isolation)

-- | Roll back the current transaction and begin a new one.
-- This rolls back to the state of the last call to 'transactionSave' or the enclosing
-- 'runSqlConn' call.
--
-- @since 1.2.0
transactionUndo :: MonadIO m => ReaderT SqlBackend m ()
transactionUndo = do
    conn <- ask
    let getter = getStmtConn conn
    liftIO $ connRollback conn getter >> connBegin conn getter Nothing

-- | Roll back the current transaction and begin a new one with the specified isolation level.
--
-- @since 2.9.0
transactionUndoWithIsolation :: MonadIO m => IsolationLevel -> ReaderT SqlBackend m ()
transactionUndoWithIsolation isolation = do
    conn <- ask
    let getter = getStmtConn conn
    liftIO $ connRollback conn getter >> connBegin conn getter (Just isolation)
