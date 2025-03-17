{-# LANGUAGE RankNTypes #-}

module Database.Persist.SqlBackend.Internal.Statement where

import Conduit
import Data.Acquire
import Data.Int
import Database.Persist.Types.Base

-- | A 'Statement' is a representation of a database query that has been
-- prepared and stored on the server side.
data Statement = Statement
    { stmtFinalize :: IO ()
    , stmtReset :: IO ()
    , stmtExecute :: [PersistValue] -> IO Int64
    , stmtQuery
        :: forall m
         . (MonadIO m)
        => [PersistValue]
        -> Acquire (ConduitM () [PersistValue] m ())
    }
