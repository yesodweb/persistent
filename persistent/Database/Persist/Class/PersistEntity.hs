{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Database.Persist.Class.PersistEntity
    ( PersistEntity (..)
    , Update (..)
    , BackendSpecificUpdate
    , SelectOpt (..)
    , Filter (..)
    , BackendSpecificFilter
    , Entity (..)

    , entityValues
    , keyValueEntityToJSON, keyValueEntityFromJSON
    , entityIdToJSON, entityIdFromJSON
      -- * PersistField based on other typeclasses
    , toPersistValueJSON, fromPersistValueJSON
    , toPersistValueEnum, fromPersistValueEnum
    ) where

import Database.Persist.Types.Base
import Database.Persist.Class.PersistField
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Data.Aeson (ToJSON (..), FromJSON (..), fromJSON, object, (.:), (.=), Value (Object))
import qualified Data.Aeson.Parser as AP
import Data.Aeson.Types (Parser,Result(Error,Success))
#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson.Text (encodeToTextBuilder)
#else
import Data.Aeson.Encode (encodeToTextBuilder)
#endif
import Data.Attoparsec.ByteString (parseOnly)
import Control.Applicative as A ((<$>), (<*>))
import Data.Monoid (mappend)
import qualified Data.HashMap.Strict as HM
import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import GHC.Generics

-- | Persistent serialized Haskell records to the database.
-- A Database 'Entity' (A row in SQL, a document in MongoDB, etc)
-- corresponds to a 'Key' plus a Haskell record.
--
-- For every Haskell record type stored in the database there is a
-- corresponding 'PersistEntity' instance. An instance of PersistEntity
-- contains meta-data for the record.  PersistEntity also helps abstract
-- over different record types. That way the same query interface can return
-- a 'PersistEntity', with each query returning different types of Haskell
-- records.
--
-- Some advanced type system capabilities are used to make this process
-- type-safe. Persistent users usually don't need to understand the class
-- associated data and functions.
class ( PersistField (Key record), ToJSON (Key record), FromJSON (Key record)
      , Show (Key record), Read (Key record), Eq (Key record), Ord (Key record))
  => PersistEntity record where
    -- | Persistent allows multiple different backends (databases).
    type PersistEntityBackend record

    -- | By default, a backend will automatically generate the key
    -- Instead you can specify a Primary key made up of unique values.
    data Key record
    -- | A lower-level key operation.
    keyToValues :: Key record -> [PersistValue]
    -- | A lower-level key operation.
    keyFromValues :: [PersistValue] -> Either Text (Key record)
    -- | A meta-operation to retrieve the 'Key' 'EntityField'.
    persistIdField :: EntityField record (Key record)

    -- | Retrieve the 'EntityDef' meta-data for the record.
    entityDef :: Monad m => m record -> EntityDef

    -- | An 'EntityField' is parameterised by the Haskell record it belongs to
    -- and the additional type of that field.
    data EntityField record :: * -> *
    -- | Return meta-data for a given 'EntityField'.
    persistFieldDef :: EntityField record typ -> FieldDef
    -- | A meta-operation to get the database fields of a record.
    toPersistFields :: record -> [SomePersistField]
    -- | A lower-level operation to convert from database values to a Haskell record.
    fromPersistValues :: [PersistValue] -> Either Text record

    -- | Unique keys besides the 'Key'.
    data Unique record
    -- | A meta operation to retrieve all the 'Unique' keys.
    persistUniqueKeys :: record -> [Unique record]
    -- | A lower level operation.
    persistUniqueToFieldNames :: Unique record -> [(HaskellName, DBName)]
    -- | A lower level operation.
    persistUniqueToValues :: Unique record -> [PersistValue]

    -- | Use a 'PersistField' as a lens.
    fieldLens :: EntityField record field
              -> (forall f. Functor f => (field -> f field) -> Entity record -> f (Entity record))

type family BackendSpecificUpdate backend record

-- | Updating a database entity.
--
-- Persistent users use combinators to create these.
data Update record = forall typ. PersistField typ => Update
    { updateField :: EntityField record typ
    , updateValue :: typ
    -- FIXME Replace with expr down the road
    , updateUpdate :: PersistUpdate
    }
    | BackendUpdate
          (BackendSpecificUpdate (PersistEntityBackend record) record)

-- | Query options.
--
-- Persistent users use these directly.
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
-- Persistent users use combinators to create these.
data Filter record = forall typ. PersistField typ => Filter
    { filterField  :: EntityField record typ
    , filterValue  :: Either typ [typ] -- FIXME
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter record] -- ^ convenient for internal use, not needed for the API
    | FilterOr  [Filter record]
    | BackendFilter
          (BackendSpecificFilter (PersistEntityBackend record) record)

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
data Entity record =
    Entity { entityKey :: Key record
           , entityVal :: record }
    deriving Typeable

deriving instance (Generic (Key record), Generic record) => Generic (Entity record)
deriving instance (Eq (Key record), Eq record) => Eq (Entity record)
deriving instance (Ord (Key record), Ord record) => Ord (Entity record)
deriving instance (Show (Key record), Show record) => Show (Entity record)
deriving instance (Read (Key record), Read record) => Read (Entity record)

-- | Get list of values corresponding to given entity.
entityValues :: PersistEntity record => Entity record -> [PersistValue]
entityValues (Entity k record) =
  if isJust (entityPrimary ent)
    then
      -- TODO: check against the key
      map toPersistValue (toPersistFields record)
    else
      keyToValues k ++ map toPersistValue (toPersistFields record)
  where
    ent = entityDef $ Just record

-- | Predefined @toJSON@. The resulting JSON looks like
-- @{"key": 1, "value": {"name": ...}}@.
--
-- The typical usage is:
--
-- @
-- instance ToJSON (Entity User) where
--     toJSON = keyValueEntityToJSON
-- @
keyValueEntityToJSON :: (PersistEntity record, ToJSON record, ToJSON (Key record))
                     => Entity record -> Value
keyValueEntityToJSON (Entity key value) = object
    [ "key" .= key
    , "value" .= value
    ]

-- | Predefined @parseJSON@. The input JSON looks like
-- @{"key": 1, "value": {"name": ...}}@.
--
-- The typical usage is:
--
-- @
-- instance FromJSON (Entity User) where
--     parseJSON = keyValueEntityFromJSON
-- @
keyValueEntityFromJSON :: (PersistEntity record, FromJSON record, FromJSON (Key record))
                       => Value -> Parser (Entity record)
keyValueEntityFromJSON (Object o) = Entity
    A.<$> o .: "key"
    A.<*> o .: "value"
keyValueEntityFromJSON _ = fail "keyValueEntityFromJSON: not an object"

-- | Predefined @toJSON@. The resulting JSON looks like
-- @{"id": 1, "name": ...}@.
--
-- The typical usage is:
--
-- @
-- instance ToJSON (Entity User) where
--     toJSON = entityIdToJSON
-- @
entityIdToJSON :: (PersistEntity record, ToJSON record, ToJSON (Key record)) => Entity record -> Value
entityIdToJSON (Entity key value) = case toJSON value of
    Object o -> Object $ HM.insert "id" (toJSON key) o
    x -> x

-- | Predefined @parseJSON@. The input JSON looks like
-- @{"id": 1, "name": ...}@.
--
-- The typical usage is:
--
-- @
-- instance FromJSON (Entity User) where
--     parseJSON = entityIdFromJSON
-- @
entityIdFromJSON :: (PersistEntity record, FromJSON record, FromJSON (Key record)) => Value -> Parser (Entity record)
entityIdFromJSON value@(Object o) = Entity <$> o .: "id" <*> parseJSON value
entityIdFromJSON _ = fail "entityIdFromJSON: not an object"

instance (PersistEntity record, PersistField record, PersistField (Key record))
  => PersistField (Entity record) where
    toPersistValue (Entity key value) = case toPersistValue value of
        (PersistMap alist) -> PersistMap ((idField, toPersistValue key) : alist)
        _ -> error $ T.unpack $ errMsg "expected PersistMap"

    fromPersistValue (PersistMap alist) = case after of
        [] -> Left $ errMsg $ "did not find " `Data.Monoid.mappend` idField `mappend` " field"
        ("_id", kv):afterRest ->
            fromPersistValue (PersistMap (before ++ afterRest)) >>= \record ->
                keyFromValues [kv] >>= \k ->
                    Right (Entity k record)
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

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with JSON instances.
--
--
-- Example usage in combination with 'fromPersistValueJSON':
--
-- @
-- instance PersistField MyData where
--   fromPersistValue = fromPersistValueJSON
--   toPersistValue = toPersistValueJSON
-- @
toPersistValueJSON :: ToJSON a => a -> PersistValue
toPersistValueJSON = PersistText . LT.toStrict . TB.toLazyText . encodeToTextBuilder . toJSON

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with JSON instances. The JSON parser used will accept JSON
-- values other that object and arrays. So, if your instance serializes the
-- data to a JSON string, this will still work.
--
--
-- Example usage in combination with 'toPersistValueJSON':
--
-- @
-- instance PersistField MyData where
--   fromPersistValue = fromPersistValueJSON
--   toPersistValue = toPersistValueJSON
-- @
fromPersistValueJSON :: FromJSON a => PersistValue -> Either Text a
fromPersistValueJSON z = case z of
  PersistByteString bs -> mapLeft (T.append "Could not parse the JSON (was a PersistByteString): ")
                        $ parseGo bs
  PersistText t -> mapLeft (T.append "Could not parse the JSON (was PersistText): ")
                 $ parseGo (TE.encodeUtf8 t)
  a -> Left $ T.append "Expected PersistByteString, received: " (T.pack (show a))
  where parseGo bs = mapLeft T.pack $ case parseOnly AP.value bs of
          Left err -> Left err
          Right v -> case fromJSON v of
            Error err -> Left err
            Success a -> Right a
        mapLeft _ (Right a) = Right a
        mapLeft f (Left b)  = Left (f b)

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with an 'Enum' instance. The function 'derivePersistField'
-- from the persistent-template package should generally be preferred.
-- However, if you want to ensure that an @ORDER BY@ clause that uses
-- your field will order rows by the data constructor order, this is
-- a better choice.
--
-- Example usage in combination with 'fromPersistValueEnum':
--
-- @
-- data SeverityLevel = Low | Medium | Critical | High
--   deriving (Enum, Bounded)
-- instance PersistField SeverityLevel where
--   fromPersistValue = fromPersistValueEnum
--   toPersistValue = toPersistValueEnum
-- @
toPersistValueEnum :: Enum a => a -> PersistValue
toPersistValueEnum = toPersistValue . fromEnum

-- | Convenience function for getting a free 'PersistField' instance
-- from a type with an 'Enum' instance. This function also requires
-- a `Bounded` instance to improve the reporting of errors.
--
-- Example usage in combination with 'toPersistValueEnum':
--
-- @
-- data SeverityLevel = Low | Medium | Critical | High
--   deriving (Enum, Bounded)
-- instance PersistField SeverityLevel where
--   fromPersistValue = fromPersistValueEnum
--   toPersistValue = toPersistValueEnum
-- @
fromPersistValueEnum :: (Enum a, Bounded a) => PersistValue -> Either Text a
fromPersistValueEnum v = fromPersistValue v >>= go
  where go i = let res = toEnum i in
               if i >= fromEnum (asTypeOf minBound res) && i <= fromEnum (asTypeOf maxBound res)
                 then Right res
                 else Left ("The number " `mappend` T.pack (show i) `mappend` " was out of the "
                  `mappend` "allowed bounds for an enum type")
