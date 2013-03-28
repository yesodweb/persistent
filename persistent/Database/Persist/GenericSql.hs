{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This is a helper module for creating SQL backends. Regular users do not
-- need to use this module.
module Database.Persist.GenericSql
    ( SqlPersist (..)
    , Connection
    , ConnectionPool
    , Statement
    , runSqlConn
    , runSqlPool
    , Key

    -- * Useful data types
    , Checkmark(..)

    -- * Raw SQL queries
    -- $rawSql
    , rawSql
    , Entity(..)
    , Single(..)
    , RawSql

    -- * Migrations
    , Migration
    , parseMigration
    , parseMigration'
    , printMigration
    , getMigration
    , runMigration
    , runMigrationSilent
    , runMigrationUnsafe
    , migrate
    , commit
    , rollback
    ) where

import qualified Prelude as P
import Prelude hiding ((++), unlines, concat, show)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Conduit.Pool
import Database.Persist.GenericSql.Migration
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception as E
import Control.Exception (throw)
import Data.Text (Text, pack, unpack, concat)
import qualified Data.Text as T
import Web.PathPieces (PathPiece (..))
import qualified Data.Text.Read
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Base (liftBase)
import Database.Persist.Types
import Database.Persist.Class
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Class
import Control.Exception.Lifted (onException)

insrepHelper :: (MonadIO m, PersistEntity val, MonadLogger m, MonadSqlPersist m)
             => Text
             -> Key val
             -> val
             -> m ()
insrepHelper command (Key k) val = do
    conn <- askSqlConn
    rawExecute (sql conn) vals
  where
    t = entityDef val
    sql conn = concat
        [ command
        , " INTO "
        , connEscapeName conn (entityDB t)
        , "("
        , T.intercalate ","
            $ map (connEscapeName conn)
            $ entityID t : map fieldDB (entityFields t)
        , ") VALUES("
        , T.intercalate "," ("?" : map (const "?") (entityFields t))
        , ")"
        ]
    vals = k : map toPersistValue (toPersistFields val)

dummyFromKey :: KeyBackend SqlBackend v -> v
dummyFromKey _ = error "dummyFromKey"

dummyFromUnique :: Unique v -> v
dummyFromUnique _ = error "dummyFromUnique"

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = mappend

show :: Show a => a -> Text
show = pack . P.show
