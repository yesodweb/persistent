{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Class.PersistEntity
    ( PersistEntity (..)
    , Update (..)
    , SelectOpt (..)
    , BackendSpecificFilter
    , Filter (..)
    , Key
    , Entity (..)
    ) where

import Database.Persist.Types.Base
import Database.Persist.Class.PersistField
import Data.Text (Text)
import Data.Aeson (ToJSON (..), FromJSON (..), object, (.:), (.=), Value (Object))
import Control.Applicative ((<$>), (<*>))

-- | A single database entity. For example, if writing a blog application, a
-- blog entry would be an entry, containing fields such as title and content.
class PersistEntity val where
    -- | Parameters: val and datatype of the field
    data EntityField val :: * -> *
    persistFieldDef :: EntityField val typ -> FieldDef

    type PersistEntityBackend val

    -- | Unique keys in existence on this entity.
    data Unique val

    entityDef :: val -> EntityDef
    toPersistFields :: val -> [SomePersistField]
    fromPersistValues :: [PersistValue] -> Either Text val
    halfDefined :: val

    persistUniqueToFieldNames :: Unique val -> [(HaskellName, DBName)]
    persistUniqueToValues :: Unique val -> [PersistValue]
    persistUniqueKeys :: val -> [Unique val]

    persistIdField :: EntityField val (Key val)

    fieldLens :: EntityField val field
              -> (forall f. Functor f => (field -> f field) -> Entity val -> f (Entity val))

data Update v = forall typ. PersistField typ => Update
    { updateField :: EntityField v typ
    , updateValue :: typ
    , updateUpdate :: PersistUpdate -- FIXME Replace with expr down the road
    }

data SelectOpt v = forall typ. Asc (EntityField v typ)
                 | forall typ. Desc (EntityField v typ)
                 | OffsetBy Int
                 | LimitTo Int

type family BackendSpecificFilter b v

-- | Filters which are available for 'select', 'updateWhere' and
-- 'deleteWhere'. Each filter constructor specifies the field being
-- filtered on, the type of comparison applied (equals, not equals, etc)
-- and the argument for the comparison.
data Filter v = forall typ. PersistField typ => Filter
    { filterField  :: EntityField v typ
    , filterValue  :: Either typ [typ] -- FIXME
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter v] -- ^ convenient for internal use, not needed for the API
    | FilterOr  [Filter v]
    | BackendFilter (BackendSpecificFilter (PersistEntityBackend v) v)

-- | Helper wrapper, equivalent to @Key (PersistEntityBackend val) val@.
--
-- Since 1.1.0
type Key val = KeyBackend (PersistEntityBackend val) val

-- | Datatype that represents an entity, with both its key and
-- its Haskell representation.
--
-- When using the an SQL-based backend (such as SQLite or
-- PostgreSQL), an 'Entity' may take any number of columns
-- depending on how many fields it has. In order to reconstruct
-- your entity on the Haskell side, @persistent@ needs all of
-- your entity columns and in the right order.  Note that you
-- don't need to worry about this when using @persistent@\'s API
-- since everything is handled correctly behind the scenes.
--
-- However, if you want to issue a raw SQL command that returns
-- an 'Entity', then you have to be careful with the column
-- order.  While you could use @SELECT Entity.* WHERE ...@ and
-- that would work most of the time, there are times when the
-- order of the columns on your database is different from the
-- order that @persistent@ expects (for example, if you add a new
-- field in the middle of you entity definition and then use the
-- migration code -- @persistent@ will expect the column to be in
-- the middle, but your DBMS will put it as the last column).
-- So, instead of using a query like the one above, you may use
-- 'Database.Persist.GenericSql.rawSql' (from the
-- "Database.Persist.GenericSql" module) with its /entity
-- selection placeholder/ (a double question mark @??@).  Using
-- @rawSql@ the query above must be written as @SELECT ??  WHERE
-- ..@.  Then @rawSql@ will replace @??@ with the list of all
-- columns that we need from your entity in the right order.  If
-- your query returns two entities (i.e. @(Entity backend a,
-- Entity backend b)@), then you must you use @SELECT ??, ??
-- WHERE ...@, and so on.
data Entity entity =
    Entity { entityKey :: Key entity
           , entityVal :: entity }
    deriving (Eq, Ord, Show, Read)

instance ToJSON e => ToJSON (Entity e) where
    toJSON (Entity k v) = object
        [ "key" .= k
        , "value" .= v
        ]
instance FromJSON e => FromJSON (Entity e) where
    parseJSON (Object o) = Entity
        <$> o .: "key"
        <*> o .: "value"
    parseJSON _ = fail "FromJSON Entity: not an object"
