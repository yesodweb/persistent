-- | An 'EntityDef' represents metadata about a type that @persistent@ uses to
-- store the type in the database, as well as generate Haskell code from it.
--
-- @since 2.13.0.0
module Database.Persist.EntityDef
    ( -- * The 'EntityDef' type
      EntityDef
      -- * Construction
      -- * Accessors
    , getEntityHaskellName
    , getEntityDBName
    , getEntityFields
    , getEntityForeignDefs
    , getEntityUniques
    , getEntityId
    , getEntityKeyFields
    , getEntityComments
    , getEntityExtra
    , isEntitySum
    , entityPrimary
    , entitiesPrimary
    , keyAndEntityFields
     -- * Setters
    , setEntityId
    , setEntityDBName
    , overEntityFields
    ) where

import Data.Text (Text)
import Data.Map (Map)

import Database.Persist.EntityDef.Internal

import Database.Persist.Types.Base
    ( UniqueDef
    , ForeignDef
    , FieldDef
    , entityKeyFields
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

getEntityExtra :: EntityDef -> Map Text [[Text]]
getEntityExtra = entityExtra

-- |
--
-- @since 2.13.0.0
setEntityDBName :: EntityNameDB -> EntityDef -> EntityDef
setEntityDBName db ed = ed { entityDB = db }

getEntityComments :: EntityDef -> Maybe Text
getEntityComments = entityComments

-- |
--
-- @since 2.13.0.0
getEntityForeignDefs
    :: EntityDef
    -> [ForeignDef]
getEntityForeignDefs = entityForeigns

-- | Retrieve the list of 'FieldDef' that makes up the fields of the entity.
--
-- This does not return the fields for an @Id@ column or an implicit @id@. It
-- will return the key columns if you used the @Primary@ syntax for defining the
-- primary key.
--
-- @since 2.13.0.0
getEntityFields
    :: EntityDef
    -> [FieldDef]
getEntityFields = entityFields

-- |
--
-- @since 2.13.0.0
isEntitySum
    :: EntityDef
    -> Bool
isEntitySum = entitySum

-- |
--
-- @since 2.13.0.0
getEntityId
    :: EntityDef
    -> FieldDef
getEntityId = entityId

setEntityId
    :: FieldDef
    -> EntityDef
    -> EntityDef
setEntityId fd ed = ed { entityId = fd }

getEntityKeyFields
    :: EntityDef
    -> [FieldDef]
getEntityKeyFields = entityKeyFields

setEntityFields :: [FieldDef] -> EntityDef -> EntityDef
setEntityFields fd ed = ed { entityFields = fd }

overEntityFields
    :: ([FieldDef] -> [FieldDef])
    -> EntityDef
    -> EntityDef
overEntityFields f ed =
    setEntityFields (f (getEntityFields ed)) ed
