{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database.Persist.Class.PersistEntity
    ( PersistEntity (..)
    , Update (..)
    , SelectOpt (..)
    , BackendSpecificFilter
    , Filter (..)
    , Key
    , Entity (..)
    , tableName

    , keyValueEntityToJSON, keyValueEntityFromJSON
    , entityIdToJSON, entityIdFromJSON
    ) where

import Database.Persist.Types.Base
import Database.Persist.Class.PersistField
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON (..), FromJSON (..), object, (.:), (.=), Value (Object))
import Data.Aeson.Types (Parser)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mappend)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import Data.Monoid ((<>))

deriving instance Eq (KeyBackend backend record)
deriving instance Ord (KeyBackend backend record)
deriving instance Read (KeyBackend backend record)
instance PersistEntity record => Show (KeyBackend backend record) where
    show key@(Key pv) = "Key " <> T.unpack (tableName (recordTypeFromKey key)) <> " " <> show pv

-- | Persistent serialized Haskell records to the database.
-- A Database 'Entity' (A row in SQL, a document in MongoDB, etc)
-- corresponds to a 'Key' plus a Haskell record.
--
-- For every Haskell record type stored in the database there is a corresponding 'PersistEntity' instance.
-- An instance of PersistEntity contains meta-data for the record.
-- PersistEntity also helps abstract over different record types.
-- That way the same query interface can return a 'PersistEntity', with each query returning different types of Haskell records.
--
-- Some advanced type system capabilities are used to make this process type-safe.
-- Persistent users usually don't need to understand the class associated data and functions.
class PersistEntity record where
    -- | An 'EntityField' is parameterised by the Haskell record it belongs to
    -- and the additional type of that field
    data EntityField record :: * -> *

    -- | return meta-data for a given 'EntityField'
    persistFieldDef :: EntityField record typ -> FieldDef SqlType

    -- | Persistent allows multiple different backends
    type PersistEntityBackend record

    -- | Unique keys besided the Key
    data Unique record

    -- | retrieve the EntityDef meta-data for the record
    entityDef :: Monad m => m record -> EntityDef SqlType

    -- | Get the database fields of a record
    toPersistFields :: record -> [SomePersistField]

    -- | Convert from database values to a Haskell record
    fromPersistValues :: [PersistValue] -> Either Text record

    persistUniqueToFieldNames :: Unique record -> [(HaskellName, DBName)]
    persistUniqueToValues :: Unique record -> [PersistValue]
    persistUniqueKeys :: record -> [Unique record]

    persistIdField :: EntityField record (Key record)

    fieldLens :: EntityField record field
              -> (forall f. Functor f => (field -> f field) -> Entity record -> f (Entity record))

-- | updataing a database entity
--
-- Persistent users use combinators to create these
data Update record = forall typ. PersistField typ => Update
    { updateField :: EntityField record typ
    , updateValue :: typ
    -- FIXME Replace with expr down the road
    , updateUpdate :: PersistUpdate
    }

-- | query options
--
-- Persistent users use these directly
data SelectOpt record = forall typ. Asc  (EntityField record typ)
                      | forall typ. Desc (EntityField record typ)
                      | OffsetBy Int
                      | LimitTo Int

type family BackendSpecificFilter backend record

-- | Filters which are available for 'select', 'updateWhere' and
-- 'deleteWhere'. Each filter constructor specifies the field being
-- filtered on, the type of comparison applied (equals, not equals, etc)
-- and the argument for the comparison.
--
-- Persistent users use combinators to create these
data Filter record = forall typ. PersistField typ => Filter
    { filterField  :: EntityField record typ
    , filterValue  :: Either typ [typ] -- FIXME
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter record] -- ^ convenient for internal use, not needed for the API
    | FilterOr  [Filter record]
    | BackendFilter
          (BackendSpecificFilter (PersistEntityBackend record) record)

-- | Helper wrapper, equivalent to @Key (PersistEntityBackend val) val@.
--
-- Since 1.1.0
type Key record = KeyBackend (PersistEntityBackend record) record

-- | Datatype that represents an entity, with both its 'Key' and
-- its Haskell record representation.
--
-- When using a SQL-based backend (such as SQLite or
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

-- | Predefined @toJSON@. The resulting JSON looks like
-- @{\"key\": 1, \"value\": {\"name\": ...}}@.
--
-- The typical usage is:
--
-- @
--   instance ToJSON User where
--       toJSON = keyValueEntityToJSON
-- @
keyValueEntityToJSON :: ToJSON e => Entity e -> Value
keyValueEntityToJSON (Entity key value) = object
    [ "key" .= key
    , "value" .= value
    ]

-- | Predefined @parseJSON@. The input JSON looks like
-- @{\"key\": 1, \"value\": {\"name\": ...}}@.
--
-- The typical usage is:
--
-- @
--   instance FromJSON User where
--       parseJSON = keyValueEntityFromJSON
-- @
keyValueEntityFromJSON :: FromJSON e => Value -> Parser (Entity e)
keyValueEntityFromJSON (Object o) = Entity
    <$> o .: "key"
    <*> o .: "value"
keyValueEntityFromJSON _ = fail "keyValueEntityFromJSON: not an object"

-- | Predefined @toJSON@. The resulting JSON looks like
-- @{\"id\": 1, \"name\": ...}@.
--
-- The typical usage is:
--
-- @
--   instance ToJSON User where
--       toJSON = entityIdToJSON
-- @
entityIdToJSON :: ToJSON e => Entity e -> Value
entityIdToJSON (Entity key value) = case toJSON value of
    Object o -> Object $ HM.insert "id" (toJSON key) o
    x -> x

-- | Predefined @parseJSON@. The input JSON looks like
-- @{\"id\": 1, \"name\": ...}@.
--
-- The typical usage is:
--
-- @
--   instance FromJSON User where
--       parseJSON = entityIdFromJSON
-- @
entityIdFromJSON :: FromJSON e => Value -> Parser (Entity e)
entityIdFromJSON value@(Object o) = Entity <$> o .: "id" <*> parseJSON value
entityIdFromJSON _ = fail "entityIdFromJSON: not an object"

instance PersistField entity => PersistField (Entity entity) where
    toPersistValue (Entity key value) = case toPersistValue value of
        (PersistMap alist) -> PersistMap ((idField, toPersistValue key) : alist)
        _ -> error $ T.unpack $ errMsg "expected PersistMap"

    fromPersistValue (PersistMap alist) = case after of
        [] -> Left $ errMsg $ "did not find " `mappend` idField `mappend` " field"
        ("_id", k):afterRest ->
            case fromPersistValue (PersistMap (before ++ afterRest)) of
                Right record -> Right $ Entity (Key k) record
                Left err     -> Left err
        _ -> Left $ errMsg $ "impossible id field: " `mappend` T.pack (show alist)
      where
        (before, after) = break ((== idField) . fst) alist

    fromPersistValue x = Left $
          errMsg "Expected PersistMap, received: " `mappend` T.pack (show x)

errMsg :: Text -> Text
errMsg = mappend "PersistField entity fromPersistValue: "

-- | Realistically this is only going to be used for MongoDB,
-- so lets use MongoDB conventions
idField :: Text
idField = "_id"

-- | the database table name for a record
tableName :: (PersistEntity record) => record -> Text
tableName = unDBName . entityDB . entityDef . Just
