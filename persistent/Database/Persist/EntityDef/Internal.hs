-- | The 'EntityDef' type, fields, and constructor are exported from this
-- module. Breaking changes to the 'EntityDef' type are not reflected in
-- the major version of the API. Please import from
-- "Database.Persist.EntityDef" instead.
--
-- If you need this module, please file a GitHub issue why.
--
-- @since 2.13.0.0
module Database.Persist.EntityDef.Internal
    ( EntityDef(..)
    , entityPrimary
    , entitiesPrimary
    , keyAndEntityFields
    , toEmbedEntityDef
    , EntityIdDef(..)
    ) where

import Database.Persist.Types.Base
