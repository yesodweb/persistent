{-# LANGUAGE ScopedTypeVariables #-}

module Database.Persist.Sql.Util
    ( parseEntityValues
    , keyAndEntityColumnNames
    , entityColumnCount
    , isIdField
    , hasNaturalKey
    , hasCompositePrimaryKey
    , dbIdColumns
    , dbIdColumnsEsc
    , dbColumns
    , updateFieldDef
    , updatePersistValue
    , mkUpdateText
    , mkUpdateText'
    , commaSeparated
    , parenWrapped
    , mkInsertValues
    , mkInsertPlaceholders
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T

import Database.Persist
       ( Entity(Entity)
       , EntityDef
       , EntityField
       , FieldDef(..)
       , FieldNameDB
       , FieldNameHS(FieldNameHS)
       , PersistEntity(..)
       , PersistUpdate(..)
       , PersistValue
       , Update(..)
       , compositeFields
       , entityPrimary
       , fieldDB
       , fieldHaskell
       , fromPersistValues
       , getEntityFields
       , getEntityKeyFields
       , keyAndEntityFields
       , keyFromValues
       , persistFieldDef
       , toPersistValue
       )

import Database.Persist.SqlBackend.Internal (SqlBackend(..))

keyAndEntityColumnNames :: EntityDef -> SqlBackend -> NonEmpty Text
keyAndEntityColumnNames ent conn =
    fmap (connEscapeFieldName conn . fieldDB) (keyAndEntityFields ent)

entityColumnCount :: EntityDef -> Int
entityColumnCount e = length (getEntityFields e)
                    + if hasNaturalKey e then 0 else 1

-- | Returns 'True' if the entity has a natural key defined with the
-- Primary keyword.
--
-- A natural key is a key that is inherent to the record, and is part of
-- the actual Haskell record. The opposite of a natural key is a "surrogate
-- key", which is not part of the normal domain object. Automatically
-- generated ID columns are the most common surrogate ID, while an email
-- address is a common natural key.
--
-- @
-- User
--     email String
--     name String
--     Primary email
--
-- Person
--     Id   UUID
--     name String
--
-- Follower
--     name String
-- @
--
-- Given these entity definitions, @User@ would return 'True', because the
-- @Primary@ keyword sets the @email@ column to be the primary key. The
-- generated Haskell type would look like this:
--
-- @
-- data User = User
--     { userEmail :: String
--     , userName :: String
--     }
-- @
--
-- @Person@ would be false. While the @Id@ syntax allows you to define
-- a custom ID type for an entity, the @Id@ column is a surrogate key.
--
-- The same is true for @Follower@. The automatically generated
-- autoincremented integer primary key is a surrogate key.
--
-- There's nothing preventing you from defining a @Primary@ definition that
-- refers to a surrogate key. This is totally fine.
--
-- @since 2.11.0
hasNaturalKey :: EntityDef -> Bool
hasNaturalKey =
    Maybe.isJust . entityPrimary

-- | Returns 'True' if the provided entity has a custom composite primary
-- key. Composite keys have multiple fields in them.
--
-- @
-- User
--     email String
--     name String
--     Primary userId
--
-- Profile
--     personId PersonId
--     email    String
--     Primary personId email
--
-- Person
--     Id   UUID
--     name String
--
-- Follower
--     name String
-- @
--
-- Given these entity definitions, only @Profile@ would return 'True',
-- because it is the only entity with multiple columns in the primary key.
-- @User@ has a single column natural key. @Person@ has a custom single
-- column surrogate key defined with @Id@. And @Follower@ has a default
-- single column surrogate key.
--
-- @since 2.11.0
hasCompositePrimaryKey :: EntityDef -> Bool
hasCompositePrimaryKey ed =
    case entityPrimary ed of
        Just cdef ->
            case compositeFields cdef of
                (_ :| _ : _) ->
                    True
                _ ->
                    False
        Nothing ->
            False

dbIdColumns :: SqlBackend -> EntityDef -> NonEmpty Text
dbIdColumns conn = dbIdColumnsEsc (connEscapeFieldName conn)

dbIdColumnsEsc :: (FieldNameDB -> Text) -> EntityDef -> NonEmpty Text
dbIdColumnsEsc esc t = fmap (esc . fieldDB) $ getEntityKeyFields t

dbColumns :: SqlBackend -> EntityDef -> NonEmpty Text
dbColumns conn =
    fmap escapeColumn . keyAndEntityFields
  where
    escapeColumn = connEscapeFieldName conn . fieldDB

parseEntityValues :: PersistEntity record
                  => EntityDef -> [PersistValue] -> Either Text (Entity record)
parseEntityValues t vals =
    case entityPrimary t of
      Just pdef ->
            let pks = fmap fieldHaskell $ compositeFields pdef
                keyvals = map snd . filter ((`elem` pks) . fst)
                        $ zip (map fieldHaskell $ getEntityFields t) vals
            in fromPersistValuesComposite' keyvals vals
      Nothing -> fromPersistValues' vals
  where
    fromPersistValues' (kpv:xs) = -- oracle returns Double
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' ->
                case keyFromValues [kpv] of
                    Left _ -> error $ "fromPersistValues': keyFromValues failed on " ++ show kpv
                    Right k -> Right (Entity k xs')


    fromPersistValues' xs = Left $ pack ("error in fromPersistValues' xs=" ++ show xs)

    fromPersistValuesComposite' keyvals xs =
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' -> case keyFromValues keyvals of
                Left err -> error $ "fromPersistValuesComposite': keyFromValues failed with error: "
                    <> T.unpack err
                Right key -> Right (Entity key xs')


isIdField
    :: forall record typ. (PersistEntity record)
    => EntityField record typ
    -> Bool
isIdField f = fieldHaskell (persistFieldDef f) == FieldNameHS "Id"

-- | Gets the 'FieldDef' for an 'Update'.
updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f
updateFieldDef BackendUpdate {} = error "updateFieldDef: did not expect BackendUpdate"

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v
updatePersistValue (BackendUpdate{}) =
    error "updatePersistValue: did not expect BackendUpdate"

commaSeparated :: [Text] -> Text
commaSeparated = T.intercalate ", "

mkUpdateText :: PersistEntity record => SqlBackend -> Update record -> Text
mkUpdateText conn = mkUpdateText' (connEscapeFieldName conn) id

-- TODO: incorporate the table names into a sum type
mkUpdateText' :: PersistEntity record => (FieldNameDB -> Text) -> (Text -> Text) -> Update record -> Text
mkUpdateText' escapeName refColumn x =
  case updateUpdate x of
    Assign -> n <> "=?"
    Add -> T.concat [n, "=", refColumn n, "+?"]
    Subtract -> T.concat [n, "=", refColumn n, "-?"]
    Multiply -> T.concat [n, "=", refColumn n, "*?"]
    Divide -> T.concat [n, "=", refColumn n, "/?"]
    BackendSpecificUpdate up ->
      error . T.unpack $ "mkUpdateText: BackendSpecificUpdate " <> up <> " not supported"
  where
    n = escapeName . fieldDB . updateFieldDef $ x

parenWrapped :: Text -> Text
parenWrapped t = T.concat ["(", t, ")"]

-- | Make a list 'PersistValue' suitable for database inserts. Pairs nicely
-- with the function 'mkInsertPlaceholders'.
--
-- Does not include generated columns.
--
-- @since 2.11.0.0
mkInsertValues
    :: PersistEntity rec
    => rec
    -> [PersistValue]
mkInsertValues entity =
    Maybe.catMaybes
        . zipWith redactGeneratedCol (getEntityFields . entityDef $ Just entity)
        . map toPersistValue
        $ toPersistFields entity
  where
    redactGeneratedCol fd pv = case fieldGenerated fd of
        Nothing ->
            Just pv
        Just _ ->
            Nothing

-- | Returns a list of escaped field names and @"?"@ placeholder values for
-- performing inserts. This does not include generated columns.
--
-- Does not include generated columns.
--
-- @since 2.11.0.0
mkInsertPlaceholders
    :: EntityDef
    -> (FieldNameDB -> Text)
    -- ^ An `escape` function
    -> [(Text, Text)]
mkInsertPlaceholders ed escape =
    Maybe.mapMaybe redactGeneratedCol (getEntityFields ed)
  where
    redactGeneratedCol fd = case fieldGenerated fd of
        Nothing ->
            Just (escape (fieldDB fd), "?")
        Just _ ->
            Nothing
