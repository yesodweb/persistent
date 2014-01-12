{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Persist.Types.Base where

import qualified Data.Aeson as A
import Control.Exception (Exception)
import Web.PathPieces (PathPiece (..))
import Control.Monad.Trans.Error (Error (..))
import Data.Typeable (Typeable)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Attoparsec.Number as AN
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Vector as V
import Control.Arrow (second)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Int (Int64)
import qualified Data.Text.Read
import Data.ByteString (ByteString)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC, zonedTimeToLocalTime, zonedTimeZone)
import Data.Map (Map)
import qualified Data.HashMap.Strict as HM
import Data.Word (Word32)
#if MIN_VERSION_aeson(0, 7, 0)
import qualified Data.Scientific
#endif

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

instance PathPiece Checkmark where
    toPathPiece = pack . show
    fromPathPiece txt =
      case reads (T.unpack txt) of
        [(a, "")] -> Just a
        _         -> Nothing

data IsNullable = Nullable !WhyNullable
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

data EntityDef sqlType = EntityDef
    { entityHaskell :: !HaskellName
    , entityDB      :: !DBName
    , entityID      :: !DBName
    , entityAttrs   :: ![Attr]
    , entityFields  :: ![FieldDef sqlType]
    , entityUniques :: ![UniqueDef]
    , entityDerives :: ![Text]
    , entityExtra   :: !(Map Text [ExtraLine])
    , entitySum     :: !Bool
    }
    deriving (Show, Eq, Read, Ord, Functor)

type ExtraLine = [Text]

newtype HaskellName = HaskellName { unHaskellName :: Text }
    deriving (Show, Eq, Read, Ord)
newtype DBName = DBName { unDBName :: Text }
    deriving (Show, Eq, Read, Ord)

type Attr = Text

data FieldType
    = FTTypeCon (Maybe Text) Text
      -- ^ Optional module and name.
    | FTApp FieldType FieldType
    | FTList FieldType
  deriving (Show, Eq, Read, Ord)

data FieldDef sqlType = FieldDef
    { fieldHaskell  :: !HaskellName -- ^ name of the field
    , fieldDB       :: !DBName
    , fieldType     :: !FieldType
    , fieldSqlType  :: !sqlType
    , fieldAttrs    :: ![Attr]   -- ^ user annotations for a field
    , fieldStrict   :: !Bool      -- ^ a strict field in the data type. Default: true
    , fieldEmbedded :: Maybe (EntityDef ()) -- ^ indicates that the field uses an embedded entity
    }
    deriving (Show, Eq, Read, Ord, Functor)

data UniqueDef = UniqueDef
    { uniqueHaskell :: !HaskellName
    , uniqueDBName  :: !DBName
    , uniqueFields  :: ![(HaskellName, DBName)]
    , uniqueAttrs   :: ![Attr]
    }
    deriving (Show, Eq, Read, Ord)

data PersistException
  = PersistError Text -- ^ Generic Exception
  | PersistMarshalError Text
  | PersistInvalidField Text
  | PersistForeignConstraintUnmet Text
  | PersistMongoDBError Text
  | PersistMongoDBUnsupported Text
    deriving (Show, Typeable)

instance Exception PersistException
instance Error PersistException where
    strMsg = PersistError . pack

-- | Avoid orphan instances.
newtype ZT = ZT ZonedTime deriving (Show, Read, Typeable)

instance Eq ZT where
    ZT a /= ZT b = zonedTimeToLocalTime a /= zonedTimeToLocalTime b || zonedTimeZone a /= zonedTimeZone b
instance Ord ZT where
    ZT a `compare` ZT b = zonedTimeToUTC a `compare` zonedTimeToUTC b

-- | A raw value which can be stored in any backend and can be marshalled to
-- and from a 'PersistField'.
data PersistValue = PersistText Text
                  | PersistByteString ByteString
                  | PersistInt64 Int64
                  | PersistDouble Double
                  | PersistRational Rational
                  | PersistBool Bool
                  | PersistDay Day
                  | PersistTimeOfDay TimeOfDay
                  | PersistUTCTime UTCTime
                  | PersistZonedTime ZT
                  | PersistNull
                  | PersistList [PersistValue]
                  | PersistMap [(Text, PersistValue)]
                  | PersistObjectId ByteString -- ^ intended especially for MongoDB backend
    deriving (Show, Read, Eq, Typeable, Ord)

instance PathPiece PersistValue where
    fromPathPiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, t')
                | T.null t' -> Just $ PersistInt64 i
            _ -> Just $ PersistText t
    toPathPiece x =
        case fromPersistValueText x of
            Left e -> error e
            Right y -> y

fromPersistValueText :: PersistValue -> Either String Text
fromPersistValueText (PersistText s) = Right s
fromPersistValueText (PersistByteString bs) =
    Right $ TE.decodeUtf8With lenientDecode bs
fromPersistValueText (PersistInt64 i) = Right $ T.pack $ show i
fromPersistValueText (PersistDouble d) = Right $ T.pack $ show d
fromPersistValueText (PersistRational r) = Right $ T.pack $ show r
fromPersistValueText (PersistDay d) = Right $ T.pack $ show d
fromPersistValueText (PersistTimeOfDay d) = Right $ T.pack $ show d
fromPersistValueText (PersistUTCTime d) = Right $ T.pack $ show d
fromPersistValueText (PersistZonedTime (ZT z)) = Right $ T.pack $ show z
fromPersistValueText PersistNull = Left "Unexpected null"
fromPersistValueText (PersistBool b) = Right $ T.pack $ show b
fromPersistValueText (PersistList _) = Left "Cannot convert PersistList to Text"
fromPersistValueText (PersistMap _) = Left "Cannot convert PersistMap to Text"
fromPersistValueText (PersistObjectId _) = Left "Cannot convert PersistObjectId to Text"

instance A.ToJSON PersistValue where
    toJSON (PersistText t) = A.String $ T.cons 's' t
    toJSON (PersistByteString b) = A.String $ T.cons 'b' $ TE.decodeUtf8 $ B64.encode b
    toJSON (PersistInt64 i) = A.Number $ fromIntegral i
    toJSON (PersistDouble d) = A.Number $
#if MIN_VERSION_aeson(0, 7, 0)
        Data.Scientific.fromFloatDigits
#else
        AN.D
#endif
        d
    toJSON (PersistRational r) = A.String $ T.pack $ 'r' : show r
    toJSON (PersistBool b) = A.Bool b
    toJSON (PersistTimeOfDay t) = A.String $ T.pack $ 't' : show t
    toJSON (PersistUTCTime u) = A.String $ T.pack $ 'u' : show u
    toJSON (PersistZonedTime z) = A.String $ T.pack $ 'z' : show z
    toJSON (PersistDay d) = A.String $ T.pack $ 'd' : show d
    toJSON PersistNull = A.Null
    toJSON (PersistList l) = A.Array $ V.fromList $ map A.toJSON l
    toJSON (PersistMap m) = A.object $ map (second A.toJSON) m
    toJSON (PersistObjectId o) = A.String $ T.cons 'o' $ TE.decodeUtf8 $ B64.encode o

instance A.FromJSON PersistValue where
    parseJSON (A.String t0) =
        case T.uncons t0 of
            Nothing -> fail "Null string"
            Just ('s', t) -> return $ PersistText t
            Just ('b', t) -> either (fail "Invalid base64") (return . PersistByteString)
                           $ B64.decode $ TE.encodeUtf8 t
            Just ('t', t) -> fmap PersistTimeOfDay $ readMay t
            Just ('u', t) -> fmap PersistUTCTime $ readMay t
            Just ('z', t) -> fmap PersistZonedTime $ readMay t
            Just ('d', t) -> fmap PersistDay $ readMay t
            Just ('r', t) -> fmap PersistRational $ readMay t
            Just ('o', t) -> either (fail "Invalid base64") (return . PersistObjectId)
                           $ B64.decode $ TE.encodeUtf8 t
            Just (c, _) -> fail $ "Unknown prefix: " ++ [c]
      where
        readMay :: (Read a, Monad m) => T.Text -> m a
        readMay t =
            case reads $ T.unpack t of
                (x, _):_ -> return x
                [] -> fail "Could not read"

#if MIN_VERSION_aeson(0, 7, 0)
    parseJSON (A.Number n) = return $
        if fromInteger (floor n) == n
            then PersistInt64 $ floor n
            else PersistDouble $ fromRational $ toRational n
#else
    parseJSON (A.Number (AN.I i)) = return $ PersistInt64 $ fromInteger i
    parseJSON (A.Number (AN.D d)) = return $ PersistDouble d
#endif
    parseJSON (A.Bool b) = return $ PersistBool b
    parseJSON A.Null = return $ PersistNull
    parseJSON (A.Array a) = fmap PersistList (mapM A.parseJSON $ V.toList a)
    parseJSON (A.Object o) =
        fmap PersistMap $ mapM go $ HM.toList o
      where
        go (k, v) = fmap ((,) k) $ A.parseJSON v

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
             | SqlDayTime
             | SqlDayTimeZoned
             | SqlBlob
             | SqlOther T.Text -- ^ a backend-specific name
    deriving (Show, Read, Eq, Typeable, Ord)

newtype KeyBackend backend entity = Key { unKey :: PersistValue }
    deriving (Show, Read, Eq, Ord)

type family KeyEntity key
type instance KeyEntity (KeyBackend backend entity) = entity

instance A.ToJSON (KeyBackend backend entity) where
    toJSON (Key val) = A.toJSON val

instance A.FromJSON (KeyBackend backend entity) where
    parseJSON = fmap Key . A.parseJSON

data PersistFilter = Eq | Ne | Gt | Lt | Ge | Le | In | NotIn
                   | BackendSpecificFilter T.Text
    deriving (Read, Show)

data UpdateGetException = KeyNotFound String
    deriving Typeable
instance Show UpdateGetException where
    show (KeyNotFound key) = "Key not found during updateGet: " ++ key
instance Exception UpdateGetException

data PersistUpdate = Assign | Add | Subtract | Multiply | Divide -- FIXME need something else here
    deriving (Read, Show, Enum, Bounded)
