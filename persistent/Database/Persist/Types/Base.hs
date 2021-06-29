{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.Types.Base
    ( module Database.Persist.Types.Base
    -- * Re-exports
    , PersistValue(..)
    , fromPersistValueText
    , LiteralType(..)
    ) where

import Control.Exception (Exception)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Language.Haskell.TH.Syntax (Lift(..))
import Web.HttpApiData
       ( FromHttpApiData(..)
       , ToHttpApiData(..)
       , parseBoundedTextData
       , showTextData
       )
import Web.PathPieces (PathPiece(..))
    -- Bring `Lift (Map k v)` instance into scope, as well as `Lift Text`
    -- instance on pre-1.2.4 versions of `text`
import Instances.TH.Lift ()

import Database.Persist.Names
import Database.Persist.PersistValue

-- | A 'Checkmark' should be used as a field type whenever a
-- uniqueness constraint should guarantee that a certain kind of
-- record may appear at most once, but other kinds of records may
-- appear any number of times.
--
-- /NOTE:/ You need to mark any @Checkmark@ fields as @nullable@
-- (see the following example).
--
-- For example, suppose there's a @Location@ entity that
-- represents where a user has lived:
--
-- @
-- Location
--     user    UserId
--     name    Text
--     current Checkmark nullable
--
--     UniqueLocation user current
-- @
--
-- The @UniqueLocation@ constraint allows any number of
-- 'Inactive' @Location@s to be @current@.  However, there may be
-- at most one @current@ @Location@ per user (i.e., either zero
-- or one per user).
--
-- This data type works because of the way that SQL treats
-- @NULL@able fields within uniqueness constraints.  The SQL
-- standard says that @NULL@ values should be considered
-- different, so we represent 'Inactive' as SQL @NULL@, thus
-- allowing any number of 'Inactive' records.  On the other hand,
-- we represent 'Active' as @TRUE@, so the uniqueness constraint
-- will disallow more than one 'Active' record.
--
-- /Note:/ There may be DBMSs that do not respect the SQL
-- standard's treatment of @NULL@ values on uniqueness
-- constraints, please check if this data type works before
-- relying on it.
--
-- The SQL @BOOLEAN@ type is used because it's the smallest data
-- type available.  Note that we never use @FALSE@, just @TRUE@
-- and @NULL@.  Provides the same behavior @Maybe ()@ would if
-- @()@ was a valid 'PersistField'.
data Checkmark = Active
                 -- ^ When used on a uniqueness constraint, there
                 -- may be at most one 'Active' record.
               | Inactive
                 -- ^ When used on a uniqueness constraint, there
                 -- may be any number of 'Inactive' records.
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToHttpApiData Checkmark where
    toUrlPiece = showTextData

instance FromHttpApiData Checkmark where
    parseUrlPiece = parseBoundedTextData

instance PathPiece Checkmark where
  toPathPiece Active = "active"
  toPathPiece Inactive = "inactive"

  fromPathPiece "active" = Just Active
  fromPathPiece "inactive" = Just Inactive
  fromPathPiece _ = Nothing

data IsNullable
    = Nullable !WhyNullable
    | NotNullable
    deriving (Eq, Show)

fieldAttrsContainsNullable :: [FieldAttr] -> IsNullable
fieldAttrsContainsNullable s
    | FieldAttrMaybe    `elem` s = Nullable ByMaybeAttr
    | FieldAttrNullable `elem` s = Nullable ByNullableAttr
    | otherwise = NotNullable

-- | The reason why a field is 'nullable' is very important.  A
-- field that is nullable because of a @Maybe@ tag will have its
-- type changed from @A@ to @Maybe A@.  OTOH, a field that is
-- nullable because of a @nullable@ tag will remain with the same
-- type.
data WhyNullable = ByMaybeAttr
                 | ByNullableAttr
                  deriving (Eq, Show)

-- | An 'EntityDef' represents the information that @persistent@ knows
-- about an Entity. It uses this information to generate the Haskell
-- datatype, the SQL migrations, and other relevant conversions.
data EntityDef = EntityDef
    { entityHaskell :: !EntityNameHS
    -- ^ The name of the entity as Haskell understands it.
    , entityDB      :: !EntityNameDB
    -- ^ The name of the database table corresponding to the entity.
    , entityId      :: !EntityIdDef
    -- ^ The entity's primary key or identifier.
    , entityAttrs   :: ![Attr]
    -- ^ The @persistent@ entity syntax allows you to add arbitrary 'Attr's
    -- to an entity using the @!@ operator. Those attributes are stored in
    -- this list.
    , entityFields  :: ![FieldDef]
    -- ^ The fields for this entity. Note that the ID field will not be
    -- present in this list. To get all of the fields for an entity, use
    -- 'keyAndEntityFields'.
    , entityUniques :: ![UniqueDef]
    -- ^ The Uniqueness constraints for this entity.
    , entityForeigns:: ![ForeignDef]
    -- ^ The foreign key relationships that this entity has to other
    -- entities.
    , entityDerives :: ![Text]
    -- ^ A list of type classes that have been derived for this entity.
    , entityExtra   :: !(Map Text [ExtraLine])
    , entitySum     :: !Bool
    -- ^ Whether or not this entity represents a sum type in the database.
    , entityComments :: !(Maybe Text)
    -- ^ Optional comments on the entity.
    --
    -- @since 2.10.0
    }
    deriving (Show, Eq, Read, Ord, Lift)

-- | The definition for the entity's primary key ID.
--
-- @since 2.13.0.0
data EntityIdDef
    = EntityIdField !FieldDef
    -- ^ The entity has a single key column, and it is a surrogate key - that
    -- is, you can't go from @rec -> Key rec@.
    --
    -- @since 2.13.0.0
    | EntityIdNaturalKey !CompositeDef
    -- ^ The entity has a natural key. This means you can write @rec -> Key rec@
    -- because all the key fields are present on the datatype.
    --
    -- A natural key can have one or more columns.
    --
    -- @since 2.13.0.0
    deriving (Show, Eq, Read, Ord, Lift)

-- | Return the @['FieldDef']@ for the entity keys.
entitiesPrimary :: EntityDef -> NonEmpty FieldDef
entitiesPrimary t =
    case entityId t of
        EntityIdNaturalKey fds ->
            compositeFields fds
        EntityIdField fd ->
            pure fd

entityPrimary :: EntityDef -> Maybe CompositeDef
entityPrimary t =
    case entityId t of
        EntityIdNaturalKey c ->
            Just c
        _ ->
            Nothing

entityKeyFields :: EntityDef -> NonEmpty FieldDef
entityKeyFields =
    entitiesPrimary

-- | Returns a 'NonEmpty' list of 'FieldDef' that correspond with the key
-- columns for an 'EntityDef'.
keyAndEntityFields :: EntityDef -> NonEmpty FieldDef
keyAndEntityFields ent =
    case entityId ent of
        EntityIdField fd ->
            fd :| fields
        EntityIdNaturalKey _ ->
            case NEL.nonEmpty fields of
                Nothing ->
                    error $ mconcat
                        [ "persistent internal guarantee failed: entity is "
                        , "defined with an entityId = EntityIdNaturalKey, "
                        , "but somehow doesn't have any entity fields."
                        ]
                Just xs ->
                    xs
  where
    fields = filter isHaskellField $ entityFields ent

type ExtraLine = [Text]

type Attr = Text

-- | Attributes that may be attached to fields that can affect migrations
-- and serialization in backend-specific ways.
--
-- While we endeavor to, we can't forsee all use cases for all backends,
-- and so 'FieldAttr' is extensible through its constructor 'FieldAttrOther'.
--
-- @since 2.11.0.0
data FieldAttr
    = FieldAttrMaybe
    -- ^ The 'Maybe' keyword goes after the type. This indicates that the column
    -- is nullable, and the generated Haskell code will have a @'Maybe'@ type
    -- for it.
    --
    -- Example:
    --
    -- @
    -- User
    --     name Text Maybe
    -- @
    | FieldAttrNullable
    -- ^ This indicates that the column is nullable, but should not have
    -- a 'Maybe' type. For this to work out, you need to ensure that the
    -- 'PersistField' instance for the type in question can support
    -- a 'PersistNull' value.
    --
    -- @
    -- data What = NoWhat | Hello Text
    --
    -- instance PersistField What where
    --     fromPersistValue PersistNull =
    --         pure NoWhat
    --     fromPersistValue pv =
    --         Hello <$> fromPersistValue pv
    --
    -- instance PersistFieldSql What where
    --     sqlType _ = SqlString
    --
    -- User
    --     what What nullable
    -- @
    | FieldAttrMigrationOnly
    -- ^ This tag means that the column will not be present on the Haskell code,
    -- but will not be removed from the database. Useful to deprecate fields in
    -- phases.
    --
    -- You should set the column to be nullable in the database. Otherwise,
    -- inserts won't have values.
    --
    -- @
    -- User
    --     oldName Text MigrationOnly
    --     newName Text
    -- @
    | FieldAttrSafeToRemove
    -- ^ A @SafeToRemove@ attribute is not present on the Haskell datatype, and
    -- the backend migrations should attempt to drop the column without
    -- triggering any unsafe migration warnings.
    --
    -- Useful after you've used @MigrationOnly@ to remove a column from the
    -- database in phases.
    --
    -- @
    -- User
    --     oldName Text SafeToRemove
    --     newName Text
    -- @
    | FieldAttrNoreference
    -- ^ This attribute indicates that we should create a foreign key reference
    -- from a column. By default, @persistent@ will try and create a foreign key
    -- reference for a column if it can determine that the type of the column is
    -- a @'Key' entity@ or an @EntityId@  and the @Entity@'s name was present in
    -- 'mkPersist'.
    --
    -- This is useful if you want to use the explicit foreign key syntax.
    --
    -- @
    -- Post
    --     title    Text
    --
    -- Comment
    --     postId   PostId      noreference
    --     Foreign Post fk_comment_post postId
    -- @
    | FieldAttrReference Text
    -- ^ This is set to specify precisely the database table the column refers
    -- to.
    --
    -- @
    -- Post
    --     title    Text
    --
    -- Comment
    --     postId   PostId references="post"
    -- @
    --
    -- You should not need this - @persistent@ should be capable of correctly
    -- determining the target table's name. If you do need this, please file an
    -- issue describing why.
    | FieldAttrConstraint Text
    -- ^ Specify a name for the constraint on the foreign key reference for this
    -- table.
    --
    -- @
    -- Post
    --     title    Text
    --
    -- Comment
    --     postId   PostId constraint="my_cool_constraint_name"
    -- @
    | FieldAttrDefault Text
    -- ^ Specify the default value for a column.
    --
    -- @
    -- User
    --     createdAt    UTCTime     default="NOW()"
    -- @
    --
    -- Note that a @default=@ attribute does not mean you can omit the value
    -- while inserting.
    | FieldAttrSqltype Text
    -- ^ Specify a custom SQL type for the column. Generally, you should define
    -- a custom datatype with a custom 'PersistFieldSql' instance instead of
    -- using this.
    --
    -- @
    -- User
    --     uuid     Text    sqltype="UUID"
    -- @
    | FieldAttrMaxlen Integer
    -- ^ Set a maximum length for a column. Useful for VARCHAR and indexes.
    --
    -- @
    -- User
    --     name     Text    maxlen=200
    --
    --     UniqueName name
    -- @
    | FieldAttrSql Text
    -- ^ Specify the database name of the column.
    --
    -- @
    -- User
    --     blarghle     Int     sql="b_l_a_r_g_h_l_e"
    -- @
    --
    -- Useful for performing phased migrations, where one column is renamed to
    -- another column over time.
    | FieldAttrOther Text
    -- ^ A grab bag of random attributes that were unrecognized by the parser.
    deriving (Show, Eq, Read, Ord, Lift)

-- | Parse raw field attributes into structured form. Any unrecognized
-- attributes will be preserved, identically as they are encountered,
-- as 'FieldAttrOther' values.
--
-- @since 2.11.0.0
parseFieldAttrs :: [Text] -> [FieldAttr]
parseFieldAttrs = fmap $ \case
    "Maybe" -> FieldAttrMaybe
    "nullable" -> FieldAttrNullable
    "MigrationOnly" -> FieldAttrMigrationOnly
    "SafeToRemove" -> FieldAttrSafeToRemove
    "noreference" -> FieldAttrNoreference
    raw
        | Just x <- T.stripPrefix "reference=" raw -> FieldAttrReference x
        | Just x <- T.stripPrefix "constraint=" raw -> FieldAttrConstraint x
        | Just x <- T.stripPrefix "default=" raw -> FieldAttrDefault x
        | Just x <- T.stripPrefix "sqltype=" raw -> FieldAttrSqltype x
        | Just x <- T.stripPrefix "maxlen=" raw -> case reads (T.unpack x) of
            [(n, s)] | all isSpace s -> FieldAttrMaxlen n
            _ -> error $ "Could not parse maxlen field with value " <> show raw
        | Just x <- T.stripPrefix "sql=" raw ->
            FieldAttrSql x
        | otherwise -> FieldAttrOther raw

-- | A 'FieldType' describes a field parsed from the QuasiQuoter and is
-- used to determine the Haskell type in the generated code.
--
-- @name Text@ parses into @FTTypeCon Nothing "Text"@
--
-- @name T.Text@ parses into @FTTypeCon (Just "T" "Text")@
--
-- @name (Jsonb User)@ parses into:
--
-- @
-- FTApp (FTTypeCon Nothing "Jsonb") (FTTypeCon Nothing "User")
-- @
data FieldType
    = FTTypeCon (Maybe Text) Text
    -- ^ Optional module and name.
    | FTTypePromoted Text
    | FTApp FieldType FieldType
    | FTList FieldType
    deriving (Show, Eq, Read, Ord, Lift)

isFieldNotGenerated :: FieldDef -> Bool
isFieldNotGenerated = isNothing . fieldGenerated

-- | There are 3 kinds of references
-- 1) composite (to fields that exist in the record)
-- 2) single field
-- 3) embedded
data ReferenceDef
    = NoReference
    | ForeignRef !EntityNameHS
    -- ^ A ForeignRef has a late binding to the EntityDef it references via name
    -- and has the Haskell type of the foreign key in the form of FieldType
    | EmbedRef EntityNameHS
    | CompositeRef CompositeDef
    | SelfReference
    -- ^ A SelfReference stops an immediate cycle which causes non-termination at compile-time (issue #311).
    deriving (Show, Eq, Read, Ord, Lift)

-- | An EmbedEntityDef is the same as an EntityDef
-- But it is only used for fieldReference
-- so it only has data needed for embedding
data EmbedEntityDef = EmbedEntityDef
    { embeddedHaskell :: EntityNameHS
    , embeddedFields  :: [EmbedFieldDef]
    } deriving (Show, Eq, Read, Ord, Lift)

-- | An EmbedFieldDef is the same as a FieldDef
-- But it is only used for embeddedFields
-- so it only has data needed for embedding
data EmbedFieldDef = EmbedFieldDef
    { emFieldDB    :: FieldNameDB
    , emFieldEmbed :: Maybe (Either SelfEmbed EntityNameHS)
    }
    deriving (Show, Eq, Read, Ord, Lift)

data SelfEmbed = SelfEmbed
    deriving (Show, Eq, Read, Ord, Lift)

-- | Returns 'True' if the 'FieldDef' does not have a 'MigrationOnly' or
-- 'SafeToRemove' flag from the QuasiQuoter.
--
-- @since 2.13.0.0
isHaskellField :: FieldDef -> Bool
isHaskellField fd =
    FieldAttrMigrationOnly `notElem` fieldAttrs fd &&
    FieldAttrSafeToRemove `notElem` fieldAttrs fd

toEmbedEntityDef :: EntityDef -> EmbedEntityDef
toEmbedEntityDef ent = embDef
  where
    embDef = EmbedEntityDef
        { embeddedHaskell = entityHaskell ent
        , embeddedFields =
            map toEmbedFieldDef
            $ filter isHaskellField
            $ entityFields ent
        }
    toEmbedFieldDef :: FieldDef -> EmbedFieldDef
    toEmbedFieldDef field =
        EmbedFieldDef
            { emFieldDB =
                fieldDB field
            , emFieldEmbed =
                case fieldReference field of
                    EmbedRef em ->
                        Just $ Right em
                    SelfReference -> Just $ Left SelfEmbed
                    _ -> Nothing
            }

-- | Type for storing the Uniqueness constraint in the Schema.  Assume you have
-- the following schema with a uniqueness constraint:
--
-- @
-- Person
--   name String
--   age Int
--   UniqueAge age
-- @
--
-- This will be represented as:
--
-- @
-- UniqueDef
--     { uniqueHaskell = ConstraintNameHS (packPTH "UniqueAge")
--     , uniqueDBName = ConstraintNameDB (packPTH "unique_age")
--     , uniqueFields = [(FieldNameHS (packPTH "age"), FieldNameDB (packPTH "age"))]
--     , uniqueAttrs = []
--     }
-- @
--
data UniqueDef = UniqueDef
    { uniqueHaskell :: !ConstraintNameHS
    , uniqueDBName  :: !ConstraintNameDB
    , uniqueFields  :: !(NonEmpty (FieldNameHS, FieldNameDB))
    , uniqueAttrs   :: ![Attr]
    }
    deriving (Show, Eq, Read, Ord, Lift)

data CompositeDef = CompositeDef
    { compositeFields  :: !(NonEmpty FieldDef)
    , compositeAttrs   :: ![Attr]
    }
    deriving (Show, Eq, Read, Ord, Lift)

-- | Used instead of FieldDef
-- to generate a smaller amount of code
type ForeignFieldDef = (FieldNameHS, FieldNameDB)

data ForeignDef = ForeignDef
    { foreignRefTableHaskell       :: !EntityNameHS
    , foreignRefTableDBName        :: !EntityNameDB
    , foreignConstraintNameHaskell :: !ConstraintNameHS
    , foreignConstraintNameDBName  :: !ConstraintNameDB
    , foreignFieldCascade          :: !FieldCascade
    -- ^ Determine how the field will cascade on updates and deletions.
    --
    -- @since 2.11.0
    , foreignFields                :: ![(ForeignFieldDef, ForeignFieldDef)] -- this entity plus the primary entity
    , foreignAttrs                 :: ![Attr]
    , foreignNullable              :: Bool
    , foreignToPrimary             :: Bool
    -- ^ Determines if the reference is towards a Primary Key or not.
    --
    -- @since 2.11.0
    }
    deriving (Show, Eq, Read, Ord, Lift)

-- | This datatype describes how a foreign reference field cascades deletes
-- or updates.
--
-- This type is used in both parsing the model definitions and performing
-- migrations. A 'Nothing' in either of the field values means that the
-- user has not specified a 'CascadeAction'. An unspecified 'CascadeAction'
-- is defaulted to 'Restrict' when doing migrations.
--
-- @since 2.11.0
data FieldCascade = FieldCascade
    { fcOnUpdate :: !(Maybe CascadeAction)
    , fcOnDelete :: !(Maybe CascadeAction)
    }
    deriving (Show, Eq, Read, Ord, Lift)

-- | A 'FieldCascade' that does nothing.
--
-- @since 2.11.0
noCascade :: FieldCascade
noCascade = FieldCascade Nothing Nothing

-- | Renders a 'FieldCascade' value such that it can be used in SQL
-- migrations.
--
-- @since 2.11.0
renderFieldCascade :: FieldCascade -> Text
renderFieldCascade (FieldCascade onUpdate onDelete) =
    T.unwords
        [ foldMap (mappend " ON DELETE " . renderCascadeAction) onDelete
        , foldMap (mappend " ON UPDATE " . renderCascadeAction) onUpdate
        ]

-- | An action that might happen on a deletion or update on a foreign key
-- change.
--
-- @since 2.11.0
data CascadeAction = Cascade | Restrict | SetNull | SetDefault
    deriving (Show, Eq, Read, Ord, Lift)

-- | Render a 'CascadeAction' to 'Text' such that it can be used in a SQL
-- command.
--
-- @since 2.11.0
renderCascadeAction :: CascadeAction -> Text
renderCascadeAction action = case action of
  Cascade    -> "CASCADE"
  Restrict   -> "RESTRICT"
  SetNull    -> "SET NULL"
  SetDefault -> "SET DEFAULT"

data PersistException
  = PersistError Text -- ^ Generic Exception
  | PersistMarshalError Text
  | PersistInvalidField Text
  | PersistForeignConstraintUnmet Text
  | PersistMongoDBError Text
  | PersistMongoDBUnsupported Text
    deriving Show

instance Exception PersistException

-- | A SQL data type. Naming attempts to reflect the underlying Haskell
-- datatypes, eg SqlString instead of SqlVarchar. Different SQL databases may
-- have different translations for these types.
data SqlType = SqlString
             | SqlInt32
             | SqlInt64
             | SqlReal
             | SqlNumeric Word32 Word32
             | SqlBool
             | SqlDay
             | SqlTime
             | SqlDayTime -- ^ Always uses UTC timezone
             | SqlBlob
             | SqlOther T.Text -- ^ a backend-specific name
    deriving (Show, Read, Eq, Ord, Lift)

data PersistFilter = Eq | Ne | Gt | Lt | Ge | Le | In | NotIn
                   | BackendSpecificFilter T.Text
    deriving (Read, Show, Lift)

data UpdateException = KeyNotFound String
                     | UpsertError String
instance Show UpdateException where
    show (KeyNotFound key) = "Key not found during updateGet: " ++ key
    show (UpsertError msg) = "Error during upsert: " ++ msg
instance Exception UpdateException

data OnlyUniqueException = OnlyUniqueException String
instance Show OnlyUniqueException where
    show (OnlyUniqueException uniqueMsg) =
      "Expected only one unique key, got " ++ uniqueMsg
instance Exception OnlyUniqueException


data PersistUpdate
    = Assign | Add | Subtract | Multiply | Divide
    | BackendSpecificUpdate T.Text
    deriving (Read, Show, Lift)

-- | A 'FieldDef' represents the inormation that @persistent@ knows about
-- a field of a datatype. This includes information used to parse the field
-- out of the database and what the field corresponds to.
data FieldDef = FieldDef
    { fieldHaskell   :: !FieldNameHS
    -- ^ The name of the field. Note that this does not corresponds to the
    -- record labels generated for the particular entity - record labels
    -- are generated with the type name prefixed to the field, so
    -- a 'FieldDef' that contains a @'FieldNameHS' "name"@ for a type
    -- @User@ will have a record field @userName@.
    , fieldDB        :: !FieldNameDB
    -- ^ The name of the field in the database. For SQL databases, this
    -- corresponds to the column name.
    , fieldType      :: !FieldType
    -- ^ The type of the field in Haskell.
    , fieldSqlType   :: !SqlType
    -- ^ The type of the field in a SQL database.
    , fieldAttrs     :: ![FieldAttr]
    -- ^ User annotations for a field. These are provided with the @!@
    -- operator.
    , fieldStrict    :: !Bool
    -- ^ If this is 'True', then the Haskell datatype will have a strict
    -- record field. The default value for this is 'True'.
    , fieldReference :: !ReferenceDef
    , fieldCascade :: !FieldCascade
    -- ^ Defines how operations on the field cascade on to the referenced
    -- tables. This doesn't have any meaning if the 'fieldReference' is set
    -- to 'NoReference' or 'SelfReference'. The cascade option here should
    -- be the same as the one obtained in the 'fieldReference'.
    --
    -- @since 2.11.0
    , fieldComments  :: !(Maybe Text)
    -- ^ Optional comments for a 'Field'. There is not currently a way to
    -- attach comments to a field in the quasiquoter.
    --
    -- @since 2.10.0
    , fieldGenerated :: !(Maybe Text)
    -- ^ Whether or not the field is a @GENERATED@ column, and additionally
    -- the expression to use for generation.
    --
    -- @since 2.11.0.0
    , fieldIsImplicitIdColumn :: !Bool
    -- ^ 'True' if the field is an implicit ID column. 'False' otherwise.
    --
    -- @since 2.13.0.0
    }
    deriving (Show, Eq, Read, Ord, Lift)
