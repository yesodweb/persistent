{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Persist.Types.Base
    ( module Database.Persist.Types.Base
    -- * Re-exports
    , PersistValue(.., PersistLiteral, PersistLiteralEscaped, PersistDbSpecific)
    , LiteralType(..)
    ) where

import Control.Arrow (second)
import Control.Exception (Exception)
import qualified Data.Aeson as A
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString, foldl')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (isNothing)
#if !MIN_VERSION_base(4,11,0)
-- This can be removed when GHC < 8.2.2 isn't supported anymore
import Data.Semigroup ((<>))
#endif
import qualified Data.Scientific
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (Day, TimeOfDay, UTCTime)
import qualified Data.Vector as V
import Data.Word (Word32)
import Language.Haskell.TH.Syntax (Lift(..))
import Numeric (readHex, showHex)
import Web.HttpApiData
       ( FromHttpApiData(..)
       , ToHttpApiData(..)
       , parseBoundedTextData
       , parseUrlPieceMaybe
       , readTextData
       , showTextData
       )
import Web.PathPieces (PathPiece(..))
    -- Bring `Lift (Map k v)` instance into scope, as well as `Lift Text`
    -- instance on pre-1.2.4 versions of `text`
import Instances.TH.Lift ()

import Database.Persist.Names

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
    , entityId      :: !FieldDef
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

entitiesPrimary :: EntityDef -> Maybe [FieldDef]
entitiesPrimary t = case fieldReference primaryField of
    CompositeRef c -> Just $ compositeFields c
    ForeignRef _ _ -> Just [primaryField]
    _ -> Nothing
  where
    primaryField = entityId t

entityPrimary :: EntityDef -> Maybe CompositeDef
entityPrimary t = case fieldReference (entityId t) of
    CompositeRef c -> Just c
    _ -> Nothing

entityKeyFields :: EntityDef -> [FieldDef]
entityKeyFields ent =
    maybe [entityId ent] compositeFields $ entityPrimary ent

keyAndEntityFields :: EntityDef -> [FieldDef]
keyAndEntityFields ent =
  case entityPrimary ent of
    Nothing -> entityId ent : entityFields ent
    Just _  -> entityFields ent


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
    | FieldAttrNullable
    | FieldAttrMigrationOnly
    | FieldAttrSafeToRemove
    | FieldAttrNoreference
    | FieldAttrReference Text
    | FieldAttrConstraint Text
    | FieldAttrDefault Text
    | FieldAttrSqltype Text
    | FieldAttrMaxlen Integer
    | FieldAttrOther Text
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
    | FTApp FieldType FieldType
    | FTList FieldType
    deriving (Show, Eq, Read, Ord, Lift)

isFieldNotGenerated :: FieldDef -> Bool
isFieldNotGenerated = isNothing . fieldGenerated

-- | There are 3 kinds of references
-- 1) composite (to fields that exist in the record)
-- 2) single field
-- 3) embedded
data ReferenceDef = NoReference
                  | ForeignRef !EntityNameHS !FieldType
                    -- ^ A ForeignRef has a late binding to the EntityDef it references via name and has the Haskell type of the foreign key in the form of FieldType
                  | EmbedRef EmbedEntityDef
                  | CompositeRef CompositeDef
                  | SelfReference
                    -- ^ A SelfReference stops an immediate cycle which causes non-termination at compile-time (issue #311).
                  deriving (Show, Eq, Read, Ord, Lift)

-- | An EmbedEntityDef is the same as an EntityDef
-- But it is only used for fieldReference
-- so it only has data needed for embedding
data EmbedEntityDef = EmbedEntityDef
    { embeddedHaskell :: !EntityNameHS
    , embeddedFields  :: ![EmbedFieldDef]
    } deriving (Show, Eq, Read, Ord, Lift)

-- | An EmbedFieldDef is the same as a FieldDef
-- But it is only used for embeddedFields
-- so it only has data needed for embedding
data EmbedFieldDef = EmbedFieldDef
    { emFieldDB    :: !FieldNameDB
    , emFieldEmbed :: Maybe EmbedEntityDef
    , emFieldCycle :: Maybe EntityNameHS
    -- ^ 'emFieldEmbed' can create a cycle (issue #311)
    -- when a cycle is detected, 'emFieldEmbed' will be Nothing
    -- and 'emFieldCycle' will be Just
    }
    deriving (Show, Eq, Read, Ord, Lift)

toEmbedEntityDef :: EntityDef -> EmbedEntityDef
toEmbedEntityDef ent = embDef
  where
    embDef = EmbedEntityDef
      { embeddedHaskell = entityHaskell ent
      , embeddedFields = map toEmbedFieldDef $ entityFields ent
      }
    toEmbedFieldDef :: FieldDef -> EmbedFieldDef
    toEmbedFieldDef field =
      EmbedFieldDef { emFieldDB       = fieldDB field
                    , emFieldEmbed = case fieldReference field of
                        EmbedRef em -> Just em
                        SelfReference -> Just embDef
                        _ -> Nothing
                    , emFieldCycle = case fieldReference field of
                        SelfReference -> Just $ entityHaskell ent
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
    , uniqueFields  :: ![(FieldNameHS, FieldNameDB)]
    , uniqueAttrs   :: ![Attr]
    }
    deriving (Show, Eq, Read, Ord, Lift)

data CompositeDef = CompositeDef
    { compositeFields  :: ![FieldDef]
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

-- | A raw value which can be stored in any backend and can be marshalled to
-- and from a 'PersistField'.
data PersistValue
    = PersistText Text
    | PersistByteString ByteString
    | PersistInt64 Int64
    | PersistDouble Double
    | PersistRational Rational
    | PersistBool Bool
    | PersistDay Day
    | PersistTimeOfDay TimeOfDay
    | PersistUTCTime UTCTime
    | PersistNull
    | PersistList [PersistValue]
    | PersistMap [(Text, PersistValue)]
    | PersistObjectId ByteString -- ^ Intended especially for MongoDB backend
    | PersistArray [PersistValue] -- ^ Intended especially for PostgreSQL backend for text arrays
    | PersistLiteral_ LiteralType ByteString
    -- ^ This constructor is used to specify some raw literal value for the
    -- backend. The 'LiteralType' value specifies how the value should be
    -- escaped. This can be used to make special, custom types avaialable
    -- in the back end.
    --
    -- @since 2.12.0.0
    deriving (Show, Read, Eq, Ord)

-- | A type that determines how a backend should handle the literal.
--
-- @since 2.12.0.0
data LiteralType
    = Escaped
    -- ^ The accompanying value will be escaped before inserting into the
    -- database. This is the correct default choice to use.
    --
    -- @since 2.12.0.0
    | Unescaped
    -- ^ The accompanying value will not be escaped when inserting into the
    -- database. This is potentially dangerous - use this with care.
    --
    -- @since 2.12.0.0
    | DbSpecific
    -- ^ The 'DbSpecific' constructor corresponds to the legacy
    -- 'PersistDbSpecific' constructor. We need to keep this around because
    -- old databases may have serialized JSON representations that
    -- reference this. We don't want to break the ability of a database to
    -- load rows.
    --
    -- @since 2.12.0.0
    deriving (Show, Read, Eq, Ord)

-- | This pattern synonym used to be a data constructor for the
-- 'PersistValue' type. It was changed to be a pattern so that JSON-encoded
-- database values could be parsed into their corresponding values. You
-- should not use this, and instead prefer to pattern match on
-- `PersistLiteral_` directly.
--
-- If you use this, it will overlap a patern match on the 'PersistLiteral_,
-- 'PersistLiteral', and 'PersistLiteralEscaped' patterns. If you need to
-- disambiguate between these constructors, pattern match on
-- 'PersistLiteral_' directly.
--
-- @since 2.12.0.0
pattern PersistDbSpecific :: ByteString -> PersistValue
pattern PersistDbSpecific bs <- PersistLiteral_ _ bs where
    PersistDbSpecific bs = PersistLiteral_ DbSpecific bs

-- | This pattern synonym used to be a data constructor on 'PersistValue',
-- but was changed into a catch-all pattern synonym to allow backwards
-- compatiblity with database types. See the documentation on
-- 'PersistDbSpecific' for more details.
--
-- @since 2.12.0.0
pattern PersistLiteralEscaped :: ByteString -> PersistValue
pattern PersistLiteralEscaped bs <- PersistLiteral_ _ bs where
    PersistLiteralEscaped bs = PersistLiteral_ Escaped bs

-- | This pattern synonym used to be a data constructor on 'PersistValue',
-- but was changed into a catch-all pattern synonym to allow backwards
-- compatiblity with database types. See the documentation on
-- 'PersistDbSpecific' for more details.
--
-- @since 2.12.0.0
pattern PersistLiteral :: ByteString -> PersistValue
pattern PersistLiteral bs <- PersistLiteral_ _ bs where
    PersistLiteral bs = PersistLiteral_ Unescaped bs

{-# DEPRECATED PersistDbSpecific "Deprecated since 2.11 because of inconsistent escaping behavior across backends. The Postgres backend escapes these values, while the MySQL backend does not. If you are using this, please switch to 'PersistLiteral_' and provide a relevant 'LiteralType' for your conversion." #-}

instance ToHttpApiData PersistValue where
    toUrlPiece val =
        case fromPersistValueText val of
            Left  e -> error $ T.unpack e
            Right y -> y

instance FromHttpApiData PersistValue where
    parseUrlPiece input =
          PersistInt64 <$> parseUrlPiece input
      <!> PersistList  <$> readTextData input
      <!> PersistText  <$> return input
      where
        infixl 3 <!>
        Left _ <!> y = y
        x      <!> _ = x

instance PathPiece PersistValue where
  toPathPiece   = toUrlPiece
  fromPathPiece = parseUrlPieceMaybe

fromPersistValueText :: PersistValue -> Either Text Text
fromPersistValueText (PersistText s) = Right s
fromPersistValueText (PersistByteString bs) =
    Right $ TE.decodeUtf8With lenientDecode bs
fromPersistValueText (PersistInt64 i) = Right $ T.pack $ show i
fromPersistValueText (PersistDouble d) = Right $ T.pack $ show d
fromPersistValueText (PersistRational r) = Right $ T.pack $ show r
fromPersistValueText (PersistDay d) = Right $ T.pack $ show d
fromPersistValueText (PersistTimeOfDay d) = Right $ T.pack $ show d
fromPersistValueText (PersistUTCTime d) = Right $ T.pack $ show d
fromPersistValueText PersistNull = Left "Unexpected null"
fromPersistValueText (PersistBool b) = Right $ T.pack $ show b
fromPersistValueText (PersistList _) = Left "Cannot convert PersistList to Text"
fromPersistValueText (PersistMap _) = Left "Cannot convert PersistMap to Text"
fromPersistValueText (PersistObjectId _) = Left "Cannot convert PersistObjectId to Text"
fromPersistValueText (PersistArray _) = Left "Cannot convert PersistArray to Text"
fromPersistValueText (PersistLiteral_ _ _) = Left "Cannot convert PersistLiteral to Text"

instance A.ToJSON PersistValue where
    toJSON (PersistText t) = A.String $ T.cons 's' t
    toJSON (PersistByteString b) = A.String $ T.cons 'b' $ TE.decodeUtf8 $ B64.encode b
    toJSON (PersistInt64 i) = A.Number $ fromIntegral i
    toJSON (PersistDouble d) = A.Number $ Data.Scientific.fromFloatDigits d
    toJSON (PersistRational r) = A.String $ T.pack $ 'r' : show r
    toJSON (PersistBool b) = A.Bool b
    toJSON (PersistTimeOfDay t) = A.String $ T.pack $ 't' : show t
    toJSON (PersistUTCTime u) = A.String $ T.pack $ 'u' : show u
    toJSON (PersistDay d) = A.String $ T.pack $ 'd' : show d
    toJSON PersistNull = A.Null
    toJSON (PersistList l) = A.Array $ V.fromList $ map A.toJSON l
    toJSON (PersistMap m) = A.object $ map (second A.toJSON) m
    toJSON (PersistLiteral_ litTy b) =
        let encoded = TE.decodeUtf8 $ B64.encode b
            prefix =
                case litTy of
                    DbSpecific -> 'p'
                    Unescaped -> 'l'
                    Escaped -> 'e'
         in
            A.String $ T.cons prefix encoded
    toJSON (PersistArray a) = A.Array $ V.fromList $ map A.toJSON a
    toJSON (PersistObjectId o) =
      A.toJSON $ showChar 'o' $ showHexLen 8 (bs2i four) $ showHexLen 16 (bs2i eight) ""
        where
         (four, eight) = BS8.splitAt 4 o

         -- taken from crypto-api
         bs2i :: ByteString -> Integer
         bs2i bs = foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0 bs
         {-# INLINE bs2i #-}

         -- showHex of n padded with leading zeros if necessary to fill d digits
         -- taken from Data.BSON
         showHexLen :: (Show n, Integral n) => Int -> n -> ShowS
         showHexLen d n = showString (replicate (d - sigDigits n) '0') . showHex n  where
             sigDigits 0 = 1
             sigDigits n' = truncate (logBase (16 :: Double) $ fromIntegral n') + 1

instance A.FromJSON PersistValue where
    parseJSON (A.String t0) =
        case T.uncons t0 of
            Nothing -> fail "Null string"
            Just ('p', t) -> either (\_ -> fail "Invalid base64") (return . PersistDbSpecific)
                           $ B64.decode $ TE.encodeUtf8 t
            Just ('l', t) -> either (\_ -> fail "Invalid base64") (return . PersistLiteral)
                           $ B64.decode $ TE.encodeUtf8 t
            Just ('e', t) -> either (\_ -> fail "Invalid base64") (return . PersistLiteralEscaped)
                           $ B64.decode $ TE.encodeUtf8 t
            Just ('s', t) -> return $ PersistText t
            Just ('b', t) -> either (\_ -> fail "Invalid base64") (return . PersistByteString)
                           $ B64.decode $ TE.encodeUtf8 t
            Just ('t', t) -> PersistTimeOfDay <$> readMay t
            Just ('u', t) -> PersistUTCTime <$> readMay t
            Just ('d', t) -> PersistDay <$> readMay t
            Just ('r', t) -> PersistRational <$> readMay t
            Just ('o', t) -> maybe
                (fail "Invalid base64")
                (return . PersistObjectId . i2bs (8 * 12) . fst)
                $ headMay $ readHex $ T.unpack t
            Just (c, _) -> fail $ "Unknown prefix: " ++ [c]
      where
        headMay []    = Nothing
        headMay (x:_) = Just x
        readMay t =
            case reads $ T.unpack t of
                (x, _):_ -> return x
                [] -> fail "Could not read"

        -- taken from crypto-api
        -- |@i2bs bitLen i@ converts @i@ to a 'ByteString' of @bitLen@ bits (must be a multiple of 8).
        i2bs :: Int -> Integer -> BS.ByteString
        i2bs l i = BS.unfoldr (\l' -> if l' < 0 then Nothing else Just (fromIntegral (i `shiftR` l'), l' - 8)) (l-8)
        {-# INLINE i2bs #-}


    parseJSON (A.Number n) = return $
        if fromInteger (floor n) == n
            then PersistInt64 $ floor n
            else PersistDouble $ fromRational $ toRational n
    parseJSON (A.Bool b) = return $ PersistBool b
    parseJSON A.Null = return PersistNull
    parseJSON (A.Array a) = fmap PersistList (mapM A.parseJSON $ V.toList a)
    parseJSON (A.Object o) =
        fmap PersistMap $ mapM go $ HM.toList o
      where
        go (k, v) = (,) k <$> A.parseJSON v

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
    }
    deriving (Show, Eq, Read, Ord, Lift)
