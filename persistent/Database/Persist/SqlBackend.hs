-- | This module contains types and information necessary for a SQL database.
-- Database support libraries, like @persistent-postgresql@, will be responsible
-- for constructing these values.
module Database.Persist.SqlBackend
    ( -- * The type and construction
      SqlBackend
    , mkSqlBackend
    , MkSqlBackendArgs(..)
    , SqlBackendHooks
    , emptySqlBackendHooks
    -- * Utilities

    -- $utilities

    -- ** SqlBackend Getters
    , getRDBMS
    , getEscapedFieldName
    , getEscapedRawName
    , getEscapeRawNameFunction
    , getConnLimitOffset
    , getConnUpsertSql
    , getConnVault
    , getConnHooks
    -- ** SqlBackend Setters
    , setConnMaxParams
    , setConnRepsertManySql
    , setConnInsertManySql
    , setConnUpsertSql
    , setConnPutManySql
    , setConnVault
    , modifyConnVault
    , setConnHooks
    -- ** SqlBackendHooks
    ) where

import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Vault.Strict (Vault)
import Database.Persist.Class.PersistStore (BackendCompatible(..))
import Database.Persist.Names
import Database.Persist.SqlBackend.Internal
import qualified Database.Persist.SqlBackend.Internal as SqlBackend
       (SqlBackend(..))
import Database.Persist.SqlBackend.Internal.InsertSqlResult
import Database.Persist.SqlBackend.Internal.MkSqlBackend as Mk
       (MkSqlBackendArgs(..))
import Database.Persist.Types.Base

-- $utilities
--
-- The functions exported here are a bit more general than the record accessors.
-- The easiest way to use them is to provide the 'SqlBackend' directly to the
-- function. However, you can also use them in a 'ReaderT' context, and you can
-- even use them with any @backend@ type tht has a @'BackendCompatible'
-- 'SqlBackend' backend@ instance.

-- | This function can be used directly with a 'SqlBackend' to escape
-- a 'FieldNameDB'.
--
-- @
-- let conn :: SqlBackend
-- getEscapedFieldName (FieldNameDB "asdf") conn
-- @
--
-- Alternatively, you can use it in a @'ReaderT' 'SqlBackend'@ context, like
-- 'SqlPersistT':
--
-- @
-- query :: SqlPersistM Text
-- query = do
--     field <- getEscapedFieldName (FieldNameDB "asdf")
--     pure field
-- @
--
-- @since 2.13.0.0
getEscapedFieldName
    :: (BackendCompatible SqlBackend backend, MonadReader backend m)
    => FieldNameDB -> m Text
getEscapedFieldName fieldName = do
    func <- asks (SqlBackend.connEscapeFieldName . projectBackend)
    pure (func fieldName)

-- | This function can be used directly with a 'SqlBackend' to escape
-- a raw 'Text'.
--
-- @
-- let conn :: SqlBackend
-- getEscapedRawName (FieldNameDB "asdf") conn
-- @
--
-- Alternatively, you can use it in a @'ReaderT' 'SqlBackend'@ context, like
-- 'SqlPersistT':
--
-- @
-- query :: SqlPersistM Text
-- query = do
--     field <- getEscapedRawName (FieldNameDB "asdf")
--     pure field
-- @
--
-- @since 2.13.0.0
getEscapedRawName
    :: (BackendCompatible SqlBackend backend, MonadReader backend m)
    => Text -> m Text
getEscapedRawName name = do
    func <- getEscapeRawNameFunction
    pure (func name)

-- | Return the function for escaping a raw name.
--
-- @since 2.13.0.0
getEscapeRawNameFunction
    :: (BackendCompatible SqlBackend backend, MonadReader backend m)
    => m (Text -> Text)
getEscapeRawNameFunction = do
    asks (SqlBackend.connEscapeRawName . projectBackend)

-- | Decorate the given SQL query with the @(LIMIT, OFFSET)@ specified.
--
-- @since 2.13.0.0
getConnLimitOffset
    :: (BackendCompatible SqlBackend backend, MonadReader backend m)
    => (Int, Int)
    -- ^ The @(LIMIT, OFFSET)@ to put on the query.
    -> Text
    -- ^ The SQL query that the LIMIT/OFFSET clause will be attached to.
    -> m Text
getConnLimitOffset limitOffset sql = do
    func <- asks (SqlBackend.connLimitOffset . projectBackend)
    pure $ func limitOffset sql

-- | Retrieve the function for generating an upsert statement, if the backend
-- supports it.
--
-- @since 2.13.0.0
getConnUpsertSql
    :: (BackendCompatible SqlBackend backend, MonadReader backend m)
    => m (Maybe (EntityDef -> NonEmpty (FieldNameHS, FieldNameDB) -> Text -> Text))
getConnUpsertSql = do
    asks (SqlBackend.connUpsertSql . projectBackend)

-- | Retrieve the vault from the provided database backend.
--
-- @since 2.13.3.0
getConnVault
    ::  (BackendCompatible SqlBackend backend, MonadReader backend m)
    => m Vault
getConnVault = do
    asks (SqlBackend.connVault . projectBackend)

-- | Retrieve instrumentation hooks from the provided database backend.
--
-- @since 2.13.3.0
getConnHooks
    ::  (BackendCompatible SqlBackend backend, MonadReader backend m)
    => m SqlBackendHooks
getConnHooks = do
    asks (SqlBackend.connHooks . projectBackend)

-- | Get a tag displaying what database the 'SqlBackend' is for. Can be
-- used to differentiate features in downstream libraries for different
-- database backends.
-- @since 2.13.3.0
getRDBMS
    :: (BackendCompatible SqlBackend backend, MonadReader backend m)
    => m Text
getRDBMS = do
    asks (SqlBackend.connRDBMS . projectBackend)


-- | Set the maximum parameters that may be issued in a given SQL query. This
-- should be used only if the database backend have this limitation.
--
-- @since 2.13.0.0
setConnMaxParams
    :: Int
    -> SqlBackend
    -> SqlBackend
setConnMaxParams i sb =
    sb { connMaxParams = Just i }

-- | Set the 'connRepsertManySql' field on the 'SqlBackend'. This should only be
-- set by the database backend library. If this is not set, a slow default will
-- be used.
--
-- @since 2.13.0.0
setConnRepsertManySql
    :: (EntityDef -> Int -> Text)
    -> SqlBackend
    -> SqlBackend
setConnRepsertManySql mkQuery sb =
    sb { connRepsertManySql = Just mkQuery }

-- | Set the 'connInsertManySql' field on the 'SqlBackend'. This should only be
-- used by the database backend library to provide an efficient implementation
-- of a bulk insert function. If this is not set, a slow default will be used.
--
-- @since 2.13.0.0
setConnInsertManySql
    :: (EntityDef -> [[PersistValue]] -> InsertSqlResult)
    -> SqlBackend
    -> SqlBackend
setConnInsertManySql mkQuery sb =
    sb { connInsertManySql = Just mkQuery }

-- | Set the 'connUpsertSql' field on the 'SqlBackend'. This should only be used
-- by the database backend library to provide an efficient implementation of
-- a bulk insert function. If this is not set, a slow default will be used.
--
-- @since 2.13.0.0
setConnUpsertSql
    :: (EntityDef -> NonEmpty (FieldNameHS, FieldNameDB) -> Text -> Text)
    -> SqlBackend
    -> SqlBackend
setConnUpsertSql mkQuery sb =
    sb { connUpsertSql = Just mkQuery }

-- | Set the 'connPutManySql field on the 'SqlBackend'. This should only be used
-- by the database backend library to provide an efficient implementation of
-- a bulk insert function. If this is not set, a slow default will be used.
--
-- @since 2.13.0.0
setConnPutManySql
    :: (EntityDef -> Int -> Text)
    -> SqlBackend
    -> SqlBackend
setConnPutManySql  mkQuery sb =
    sb { connPutManySql = Just mkQuery }

-- | Set the vault on the provided database backend.
--
-- @since 2.13.0
setConnVault :: Vault -> SqlBackend -> SqlBackend
setConnVault vault sb =
    sb { connVault = vault }

-- | Modify the vault on the provided database backend.
--
-- @since 2.13.0
modifyConnVault :: (Vault -> Vault) -> SqlBackend  -> SqlBackend
modifyConnVault f sb =
    sb { connVault = f $ connVault sb }

-- | Set hooks on the provided database backend.
--
-- @since 2.13.0
setConnHooks :: SqlBackendHooks -> SqlBackend -> SqlBackend
setConnHooks hooks sb =
    sb { connHooks = hooks }
