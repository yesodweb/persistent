-- | An 'EntityDef' represents metadata about a type that @persistent@ uses to
-- store the type in the database, as well as generate Haskell code from it.
--
-- @since 2.13.0.0
module Database.Persist.EntityDef
    ( -- * The 'EntityDef' type
      EntityDef
      -- * Construction
      -- * Accessors
    , getEntityUniques
    , getEntityHaskellName
    , getEntityDBName
    , entitiesPrimary
    , keyAndEntityFields
     -- * Setters
    ) where

import Database.Persist.EntityDef.Internal

import Database.Persist.Types.Base
    ( UniqueDef
    )
import Database.Persist.Names

-- | Retrieve the list of 'UniqueDef' from an 'EntityDef'. This currently does
-- not include a @Primary@ key, if one is defined. A future version of
-- @persistent@ will include a @Primary@ key among the 'Unique' constructors for
-- the 'Entity'.
--
-- @since 2.13.0.0
getEntityUniques
    :: EntityDef
    -> [UniqueDef]
getEntityUniques = entityUniques

-- | Retrieve the Haskell name of the given entity.
--
-- @since 2.13.0.0
getEntityHaskellName
    :: EntityDef
    -> EntityNameHS
getEntityHaskellName = entityHaskell

-- | Return the database name for the given entity.
--
-- @since 2.13.0.0
getEntityDBName
    :: EntityDef
    -> EntityNameDB
getEntityDBName = entityDB
