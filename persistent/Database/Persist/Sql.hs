module Database.Persist.Sql
    ( module Database.Persist.Sql.Types
    , module Database.Persist.Sql.Class
    , module Database.Persist.Sql.Run
    , module Database.Persist.Sql.Migration
    , module Database.Persist
    , module Database.Persist.Sql.Orphan.PersistStore
    , rawQuery
    , rawQueryRes
    , rawExecute
    , rawExecuteCount
    , rawSql
    , deleteWhereCount
    , updateWhereCount
    , transactionSave
    , transactionUndo
    , getStmtConn
      -- * Internal
    , module Database.Persist.Sql.Internal
    , decorateSQLWithLimitOffset
    ) where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class
import Database.Persist.Sql.Run hiding (withResourceTimeout)
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Migration
import Database.Persist.Sql.Internal

import Database.Persist.Sql.Orphan.PersistQuery
import Database.Persist.Sql.Orphan.PersistStore
import Database.Persist.Sql.Orphan.PersistUnique ()
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask)

-- | Commit the current transaction and begin a new one.
--
-- @since 1.2.0
transactionSave :: MonadIO m => ReaderT (SqlBackend db) m ()
transactionSave = do
    conn <- ask
    let getter = getStmtConn conn
    liftIO $ connCommit conn getter >> connBegin conn getter

-- | Roll back the current transaction and begin a new one.
--
-- @since 1.2.0
transactionUndo :: MonadIO m => ReaderT (SqlBackend db) m ()
transactionUndo = do
    conn <- ask
    let getter = getStmtConn conn
    liftIO $ connRollback conn getter >> connBegin conn getter
