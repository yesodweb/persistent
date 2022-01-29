module Database.Persist.SqlBackend.Internal.SqlPoolHooks
  ( SqlPoolHooks(..)
  ) where
import Control.Exception (SomeException)
import Database.Persist.SqlBackend.Internal.IsolationLevel

-- | A set of hooks that may be used to alter the behaviour
-- of @runSqlPoolWithExtensibleHooks@ in a backwards-compatible
-- fashion.
data SqlPoolHooks m backend = SqlPoolHooks
    { alterBackend :: backend -> m backend
    -- ^ Alter the backend prior to executing any actions with it.
    , runBefore :: backend -> Maybe IsolationLevel -> m ()
    -- ^ Run this action immediately before the action is performed.
    , runAfter :: backend -> Maybe IsolationLevel -> m ()
    -- ^ Run this action immediately after the action is completed.
    , runOnException :: backend -> Maybe IsolationLevel -> SomeException -> m ()
    -- ^ This action is performed when an exception is received. The
    -- exception is provided as a convenience - it is rethrown once this
    -- cleanup function is complete.
    }
