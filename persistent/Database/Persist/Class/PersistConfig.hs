module Database.Persist.Class.PersistConfig
    ( PersistConfig (..)
    ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (Value (Object))
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap

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
    runPool :: MonadUnliftIO m
            => c
            -> PersistConfigBackend c m a
            -> PersistConfigPool c
            -> m a

instance
  ( PersistConfig c1
  , PersistConfig c2
  , PersistConfigPool c1 ~ PersistConfigPool c2
  , PersistConfigBackend c1 ~ PersistConfigBackend c2
  ) => PersistConfig (Either c1 c2) where
    type PersistConfigBackend (Either c1 c2) = PersistConfigBackend c1
    type PersistConfigPool (Either c1 c2) = PersistConfigPool c1

    loadConfig (Object o) =
        case HashMap.lookup "left" o of
            Just v -> Left <$> loadConfig v
            Nothing ->
                case HashMap.lookup "right" o of
                    Just v -> Right <$> loadConfig v
                    Nothing -> fail "PersistConfig for Either: need either a left or right"
    loadConfig _ = fail "PersistConfig for Either: need an object"

    createPoolConfig = either createPoolConfig createPoolConfig

    runPool (Left c) = runPool c
    runPool (Right c) = runPool c
