{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Class.PersistConfig
    ( PersistConfig (..)
    ) where

import Data.Aeson (Value)
import Data.Aeson.Types (Parser)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

-- | Represents a value containing all the configuration options for a specific
-- backend. This abstraction makes it easier to write code that can easily swap
-- backends.
class PersistConfig c where
    type PersistConfigBackend c :: (* -> *) -> * -> *
    type PersistConfigPool c

    -- | Load the config settings from a 'Value', most likely taken from a YAML
    -- config file.
    loadConfig :: Value -> Parser c

    -- | Modify the config settings based on environment variables.
    applyEnv :: c -> IO c
    applyEnv = return

    -- | Create a new connection pool based on the given config settings.
    createPoolConfig :: c -> IO (PersistConfigPool c)

    -- | Run a database action by taking a connection from the pool.
    runPool :: (MonadBaseControl IO m, MonadIO m)
            => c
            -> PersistConfigBackend c m a
            -> PersistConfigPool c
            -> m a
