{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{- You can't export a data family constructor, so there's an "unused" warning -}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.Compatible.Types
    ( Compatible(..)
    ) where

import Data.Aeson
import Database.Persist.Class
import Database.Persist.Sql.Class
import Control.Monad.Trans.Reader (withReaderT)


-- | A newtype wrapper for compatible backends, mainly useful for @DerivingVia@.
--
-- When writing a new backend that is 'BackendCompatible' with an existing backend,
-- instances for the new backend can be naturally defined in terms of the
-- instances for the existing backend.
--
-- For example, if you decide to augment the 'SqlBackend' with some additional
-- features:
--
-- @
-- data BetterSqlBackend = BetterSqlBackend { sqlBackend :: SqlBackend, ... }
--
-- instance BackendCompatible SqlBackend BetterSqlBackend where
--   projectBackend = sqlBackend
-- @
--
-- Then you can use @DerivingVia@ to automatically get instances like:
--
-- @
-- deriving via (Compatible SqlBackend BetterSqlBackend) instance PersistStoreRead BetterSqlBackend
-- deriving via (Compatible SqlBackend BetterSqlBackend) instance PersistStoreWrite BetterSqlBackend
-- ...
-- @
--
-- These instances will go through the compatible backend (in this case, 'SqlBackend')
-- for all their queries.
--
-- These instances require that both backends have the same 'BaseBackend', but
-- deriving 'HasPersistBackend' will enforce that for you.
--
-- @
-- deriving via (Compatible SqlBackend BetterSqlBackend) instance HasPersistBackend BetterSqlBackend
-- @
--
-- @since 2.12
newtype Compatible b s = Compatible { unCompatible :: s }

instance (BackendCompatible b s, HasPersistBackend b) => HasPersistBackend (Compatible b s) where
    type BaseBackend (Compatible b s) = BaseBackend b
    persistBackend = persistBackend . projectBackend @b @s . unCompatible

instance (BackendCompatible b s, PersistCore b) => PersistCore (Compatible b s) where
    -- | A newtype wrapper around @'BackendKey' b@, mainly useful for @DerivingVia@.
    --
    -- Similarly to @'Compatible' b s@, this data family instance is handy for deriving
    -- instances for @'BackendKey' s@ by defining them in terms of @'BackendKey' b@.
    --
    --
    -- For example, if you decide to augment the 'SqlBackend' with some additional
    -- features:
    --
    -- @
    -- data BetterSqlBackend = BetterSqlBackend { sqlBackend :: SqlBackend, ... }
    --
    -- instance PersistCore BetterSqlBackend where
    --   newtype BackendKey BetterSqlBackend = BSQLKey { unBSQLKey :: BackendKey (Compatible SqlBackend BetterSqlBackend) }
    -- @
    --
    -- Then you can use @DerivingVia@ to automatically get instances like:
    --
    -- @
    -- deriving via BackendKey (Compatible SqlBackend BetterSqlBackend) instance Show (BackendKey BetterSqlBackend)
    -- ...
    -- @
    --
    -- These instances will go through the compatible backend's key (in this case,
    -- @'BackendKey' 'SqlBackend'@) for all their logic.
    newtype BackendKey (Compatible b s) = CompatibleKey { unCompatibleKey :: BackendKey b }

instance (HasPersistBackend b, BackendCompatible b s, PersistStoreRead b) => PersistStoreRead (Compatible b s) where
    get = withReaderT (projectBackend @b @s . unCompatible) . get
    getMany = withReaderT (projectBackend @b @s . unCompatible) . getMany

instance (HasPersistBackend b, BackendCompatible b s, PersistQueryRead b) => PersistQueryRead (Compatible b s) where
    selectSourceRes filts opts = withReaderT (projectBackend @b @s . unCompatible) $ selectSourceRes filts opts
    selectFirst filts opts = withReaderT (projectBackend @b @s . unCompatible) $ selectFirst filts opts
    selectKeysRes filts opts = withReaderT (projectBackend @b @s . unCompatible) $ selectKeysRes filts opts
    count = withReaderT (projectBackend @b @s . unCompatible) . count
    exists = withReaderT (projectBackend @b @s . unCompatible) . exists

instance (HasPersistBackend b, BackendCompatible b s, PersistQueryWrite b) => PersistQueryWrite (Compatible b s) where
    updateWhere filts updates = withReaderT (projectBackend @b @s . unCompatible) $ updateWhere filts updates
    deleteWhere = withReaderT (projectBackend @b @s . unCompatible) . deleteWhere

instance (HasPersistBackend b, BackendCompatible b s, PersistUniqueRead b) => PersistUniqueRead (Compatible b s) where
    getBy = withReaderT (projectBackend @b @s . unCompatible) . getBy

instance (HasPersistBackend b, BackendCompatible b s, PersistStoreWrite b) => PersistStoreWrite (Compatible b s) where
    insert = withReaderT (projectBackend @b @s . unCompatible) . insert
    insert_ = withReaderT (projectBackend @b @s . unCompatible) . insert_
    insertMany = withReaderT (projectBackend @b @s . unCompatible) . insertMany
    insertMany_ = withReaderT (projectBackend @b @s . unCompatible) . insertMany_
    insertEntityMany = withReaderT (projectBackend @b @s . unCompatible) . insertEntityMany
    insertKey k = withReaderT (projectBackend @b @s . unCompatible) . insertKey k
    repsert k = withReaderT (projectBackend @b @s . unCompatible) . repsert k
    repsertMany = withReaderT (projectBackend @b @s . unCompatible) . repsertMany
    replace k = withReaderT (projectBackend @b @s . unCompatible) . replace k
    delete = withReaderT (projectBackend @b @s . unCompatible) . delete
    update k = withReaderT (projectBackend @b @s . unCompatible) . update k
    updateGet k = withReaderT (projectBackend @b @s . unCompatible) . updateGet k

instance (HasPersistBackend b, BackendCompatible b s, PersistUniqueWrite b) => PersistUniqueWrite (Compatible b s) where
    deleteBy = withReaderT (projectBackend @b @s . unCompatible) . deleteBy
    insertUnique = withReaderT (projectBackend @b @s . unCompatible) . insertUnique
    upsert rec = withReaderT (projectBackend @b @s . unCompatible) . upsert rec
    upsertBy uniq rec = withReaderT (projectBackend @b @s . unCompatible) . upsertBy uniq rec
    putMany = withReaderT (projectBackend @b @s . unCompatible) . putMany

deriving via (BackendKey b) instance (BackendCompatible b s, Show (BackendKey b)) => Show (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Read (BackendKey b)) => Read (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Eq (BackendKey b)) => Eq (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Ord (BackendKey b)) => Ord (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Num (BackendKey b)) => Num (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Integral (BackendKey b)) => Integral (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, PersistField (BackendKey b)) => PersistField (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, PersistFieldSql (BackendKey b)) => PersistFieldSql (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Real (BackendKey b)) => Real (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Enum (BackendKey b)) => Enum (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, Bounded (BackendKey b)) => Bounded (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, ToJSON (BackendKey b)) => ToJSON (BackendKey (Compatible b s))
deriving via (BackendKey b) instance (BackendCompatible b s, FromJSON (BackendKey b)) => FromJSON (BackendKey (Compatible b s))
