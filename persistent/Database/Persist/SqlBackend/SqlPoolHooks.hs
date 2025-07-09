module Database.Persist.SqlBackend.SqlPoolHooks
    ( SqlPoolHooks
    , defaultSqlPoolHooks
    , getAlterBackend
    , modifyAlterBackend
    , setAlterBackend
    , getRunBefore
    , modifyRunBefore
    , setRunBefore
    , getRunAfter
    , modifyRunAfter
    , setRunAfter
    , getRunOnException
    )
where

import Control.Exception
import Control.Monad.IO.Class
import Database.Persist.Class.PersistStore
import Database.Persist.Sql.Raw
import Database.Persist.SqlBackend.Internal
import Database.Persist.SqlBackend.Internal.IsolationLevel
import Database.Persist.SqlBackend.Internal.SqlPoolHooks

-- | Lifecycle hooks that may be altered to extend SQL pool behavior
-- in a backwards compatible fashion.
--
-- By default, the hooks have the following semantics:
--
-- - 'alterBackend' has no effect
-- - 'runBefore' begins a transaction
-- - 'runAfter' commits the current transaction
-- - 'runOnException' rolls back the current transaction
--
-- @since 2.13.3.0
defaultSqlPoolHooks
    :: (MonadIO m, BackendCompatible SqlBackend backend) => SqlPoolHooks m backend
defaultSqlPoolHooks =
    SqlPoolHooks
        { alterBackend = pure
        , runBefore = \conn mi -> do
            let
                sqlBackend = projectBackend conn
            let
                getter = getStmtConn sqlBackend
            liftIO $ connBegin sqlBackend getter mi
        , runAfter = \conn _ -> do
            let
                sqlBackend = projectBackend conn
            let
                getter = getStmtConn sqlBackend
            liftIO $ connCommit sqlBackend getter
        , runOnException = \conn _ _ -> do
            let
                sqlBackend = projectBackend conn
            let
                getter = getStmtConn sqlBackend
            liftIO $ connRollback sqlBackend getter
        }

getAlterBackend :: SqlPoolHooks m backend -> (backend -> m backend)
getAlterBackend = alterBackend

modifyAlterBackend
    :: SqlPoolHooks m backend
    -> ((backend -> m backend) -> (backend -> m backend))
    -> SqlPoolHooks m backend
modifyAlterBackend hooks f = hooks{alterBackend = f $ alterBackend hooks}

setAlterBackend
    :: SqlPoolHooks m backend -> (backend -> m backend) -> SqlPoolHooks m backend
setAlterBackend hooks f = hooks{alterBackend = f}

getRunBefore
    :: SqlPoolHooks m backend -> (backend -> Maybe IsolationLevel -> m ())
getRunBefore = runBefore

modifyRunBefore
    :: SqlPoolHooks m backend
    -> ( (backend -> Maybe IsolationLevel -> m ())
         -> (backend -> Maybe IsolationLevel -> m ())
       )
    -> SqlPoolHooks m backend
modifyRunBefore hooks f = hooks{runBefore = f $ runBefore hooks}

setRunBefore
    :: SqlPoolHooks m backend
    -> (backend -> Maybe IsolationLevel -> m ())
    -> SqlPoolHooks m backend
setRunBefore h f = h{runBefore = f}

getRunAfter
    :: SqlPoolHooks m backend -> (backend -> Maybe IsolationLevel -> m ())
getRunAfter = runAfter

modifyRunAfter
    :: SqlPoolHooks m backend
    -> ( (backend -> Maybe IsolationLevel -> m ())
         -> (backend -> Maybe IsolationLevel -> m ())
       )
    -> SqlPoolHooks m backend
modifyRunAfter hooks f = hooks{runAfter = f $ runAfter hooks}

setRunAfter
    :: SqlPoolHooks m backend
    -> (backend -> Maybe IsolationLevel -> m ())
    -> SqlPoolHooks m backend
setRunAfter hooks f = hooks{runAfter = f}

getRunOnException
    :: SqlPoolHooks m backend
    -> (backend -> Maybe IsolationLevel -> SomeException -> m ())
getRunOnException = runOnException

modifyRunOnException
    :: SqlPoolHooks m backend
    -> ( (backend -> Maybe IsolationLevel -> SomeException -> m ())
         -> (backend -> Maybe IsolationLevel -> SomeException -> m ())
       )
    -> SqlPoolHooks m backend
modifyRunOnException hooks f = hooks{runOnException = f $ runOnException hooks}

setRunOnException
    :: SqlPoolHooks m backend
    -> (backend -> Maybe IsolationLevel -> SomeException -> m ())
    -> SqlPoolHooks m backend
setRunOnException hooks f = hooks{runOnException = f}
