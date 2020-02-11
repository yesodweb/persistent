{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards, DataKinds, TypeOperators, UndecidableInstances #-}
module Database.Persist.Class.PersistField
    ( PersistField (..)
    , SomePersistField (..)
    , getPersistMap
    , OverflowNatural(..)
    ) where

import Control.Arrow (second)
import Control.Monad ((<=<))
import qualified Data.Aeson as A
import Data.ByteString.Char8 (ByteString, unpack, readInt)
import qualified Data.ByteString.Lazy as L
import Data.Fixed
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (double)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TERR
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import GHC.TypeLits

import Database.Persist.Types.Base

import Data.Time (Day(..), TimeOfDay, UTCTime,
    parseTimeM)
import Data.Time (defaultTimeLocale)

#ifdef HIGH_PRECISION_DATE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
#endif


-- | This class teaches Persistent how to take a custom type and marshal it to and from a 'PersistValue', allowing it to be stored in a database.
--
-- ==== __Examples__
--
-- ===== Simple Newtype
--
-- You can use @newtype@ to add more type safety/readability to a basis type like 'ByteString'. In these cases, just derive 'PersistField' and @PersistFieldSql@:
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- newtype HashedPassword = HashedPassword 'ByteString'
--   deriving (Eq, Show, 'PersistField', PersistFieldSql)
-- @
--
-- ===== Smart Constructor Newtype
--
-- In this example, we create a 'PersistField' instance for a newtype following the "Smart Constructor" pattern.
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving #-}
-- import qualified "Data.Text" as T
-- import qualified "Data.Char" as C
--
-- -- | An American Social Security Number
-- newtype SSN = SSN 'Text'
--  deriving (Eq, Show, PersistFieldSql)
--
-- mkSSN :: 'Text' -> 'Either' 'Text' SSN
-- mkSSN t = if (T.length t == 9) && (T.all C.isDigit t)
--  then 'Right' $ SSN t
--  else 'Left' $ "Invalid SSN: " <> t
--
-- instance 'PersistField' SSN where
--   'toPersistValue' (SSN t) = 'PersistText' t
--   'fromPersistValue' ('PersistText' t) = mkSSN t
--   -- Handle cases where the database does not give us PersistText
--   'fromPersistValue' x = 'Left' $ "File.hs: When trying to deserialize an SSN: expected PersistText, received: " <> T.pack (show x)
-- @
--
-- Tips:
--
-- * This file contain dozens of 'PersistField' instances you can look at for examples.
-- * Typically custom 'PersistField' instances will only accept a single 'PersistValue' constructor in 'fromPersistValue'.
-- * Internal 'PersistField' instances accept a wide variety of 'PersistValue's to accomodate e.g. storing booleans as integers, booleans or strings.
-- * If you're making a custom instance and using a SQL database, you'll also need @PersistFieldSql@ to specify the type of the database column.
class PersistField a where
    toPersistValue :: a -> PersistValue
    fromPersistValue :: PersistValue -> Either T.Text a

#ifndef NO_OVERLAP
instance {-# OVERLAPPING #-} PersistField [Char] where
    toPersistValue = PersistText . T.pack
    fromPersistValue (PersistText s) = Right $ T.unpack s
    fromPersistValue (PersistByteString bs) =
        Right $ T.unpack $ TE.decodeUtf8With TERR.lenientDecode bs
    fromPersistValue (PersistInt64 i) = Right $ Prelude.show i
    fromPersistValue (PersistDouble d) = Right $ Prelude.show d
    fromPersistValue (PersistRational r) = Right $ Prelude.show r
    fromPersistValue (PersistDay d) = Right $ Prelude.show d
    fromPersistValue (PersistTimeOfDay d) = Right $ Prelude.show d
    fromPersistValue (PersistUTCTime d) = Right $ Prelude.show d
    fromPersistValue PersistNull = Left $ T.pack "Unexpected null"
    fromPersistValue (PersistBool b) = Right $ Prelude.show b
    fromPersistValue (PersistList _) = Left $ T.pack "Cannot convert PersistList to String"
    fromPersistValue (PersistMap _) = Left $ T.pack "Cannot convert PersistMap to String"
    fromPersistValue (PersistDbSpecific _) = Left $ T.pack "Cannot convert PersistDbSpecific to String. See the documentation of PersistDbSpecific for an example of using a custom database type with Persistent."
    fromPersistValue (PersistArray _) = Left $ T.pack "Cannot convert PersistArray to String"
    fromPersistValue (PersistObjectId _) = Left $ T.pack "Cannot convert PersistObjectId to String"
#endif

instance PersistField ByteString where
    toPersistValue = PersistByteString
    fromPersistValue (PersistByteString bs) = Right bs
    fromPersistValue x = TE.encodeUtf8 <$> fromPersistValue x

instance PersistField T.Text where
    toPersistValue = PersistText
    fromPersistValue = fromPersistValueText

instance PersistField TL.Text where
    toPersistValue = toPersistValue . TL.toStrict
    fromPersistValue = fmap TL.fromStrict . fromPersistValue

instance PersistField Html where
    toPersistValue = PersistText . TL.toStrict . renderHtml
    fromPersistValue = fmap (preEscapedToMarkup :: T.Text -> Html) . fromPersistValue

instance PersistField Int where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int) -- oracle
    fromPersistValue x = Left $ fromPersistValueError "Int" "integer" x

instance PersistField Int8 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int8) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               Just (i,extra) -> Left $ extraInputError "Int64" bs i extra
                                               Nothing -> Left $ intParseError "Int64" bs
    fromPersistValue x = Left $ fromPersistValueError "Int8" "integer" x

instance PersistField Int16 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int16) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               Just (i,extra) -> Left $ extraInputError "Int64" bs i extra
                                               Nothing -> Left $ intParseError "Int64" bs
    fromPersistValue x = Left $ fromPersistValueError "Int16" "integer" x

instance PersistField Int32 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i)  = Right $ fromIntegral i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int32) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               Just (i,extra) -> Left $ extraInputError "Int64" bs i extra
                                               Nothing -> Left $ intParseError "Int64" bs
    fromPersistValue x = Left $ fromPersistValueError "Int32" "integer" x

instance PersistField Int64 where
    toPersistValue = PersistInt64
    fromPersistValue (PersistInt64 i)  = Right i
    fromPersistValue (PersistDouble i) = Right (truncate i :: Int64) -- oracle
    fromPersistValue (PersistByteString bs) = case readInt bs of  -- oracle
                                               Just (i,"") -> Right $ fromIntegral i
                                               Just (i,extra) -> Left $ extraInputError "Int64" bs i extra
                                               Nothing -> Left $ intParseError "Int64" bs
    fromPersistValue x = Left $ fromPersistValueError "Int64" "integer" x

extraInputError :: (Show result)
                => Text -- ^ Haskell type
                -> ByteString -- ^ Original bytestring
                -> result -- ^ Integer result
                -> ByteString -- ^  Extra bytestring
                -> Text -- ^ Error message
extraInputError haskellType original result extra = T.concat
    [ "Parsed "
    , TE.decodeUtf8 original
    , " into Haskell type `"
    , haskellType
    , "` with value"
    , T.pack $ show result
    , "but had extra input: "
    , TE.decodeUtf8 extra
    ]

intParseError :: Text -- ^ Haskell type
              -> ByteString -- ^ Original bytestring
              -> Text -- ^ Error message
intParseError haskellType original = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , " from "
    , TE.decodeUtf8 original
    ]

instance PersistField Data.Word.Word where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ fromPersistValueError "Word" "integer" x

instance PersistField Word8 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ fromPersistValueError "Word8" "integer" x

instance PersistField Word16 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ fromPersistValueError "Word16" "integer" x

instance PersistField Word32 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ fromPersistValueError "Word32" "integer" x

instance PersistField Word64 where
    toPersistValue = PersistInt64 . fromIntegral
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ fromPersistValueError "Word64" "integer" x

instance PersistField Double where
    toPersistValue = PersistDouble
    fromPersistValue (PersistDouble d) = Right d
    fromPersistValue (PersistRational r) = Right $ fromRational r
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ fromPersistValueError "Double" "double, rational, or integer" x

instance (HasResolution a) => PersistField (Fixed a) where
    toPersistValue = PersistRational . toRational
    fromPersistValue (PersistRational r) = Right $ fromRational r
    fromPersistValue (PersistText t) = case reads $ T.unpack t of --  NOTE: Sqlite can store rationals just as string
      [(a, "")] -> Right a
      _ -> Left $ "Can not read " <> t <> " as Fixed"
    fromPersistValue (PersistDouble d) = Right $ realToFrac d
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue x = Left $ fromPersistValueError "Fixed" "rational, string, double, or integer" x

instance PersistField Rational where
    toPersistValue = PersistRational
    fromPersistValue (PersistRational r) = Right r
    fromPersistValue (PersistDouble d) = Right $ toRational d
    fromPersistValue (PersistText t) = case reads $ T.unpack t of --  NOTE: Sqlite can store rationals just as string
      [(a, "")] -> Right $ toRational (a :: Pico)
      _ -> Left $ "Can not read " <> t <> " as Rational (Pico in fact)"
    fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
    fromPersistValue (PersistByteString bs) = case double $ T.cons '0' $ TE.decodeUtf8With TERR.lenientDecode bs of
                                                Right (ret,"") -> Right $ toRational ret
                                                Right (a,b) -> Left $ "Invalid bytestring[" <> T.pack (show bs) <> "]: expected a double but returned " <> T.pack (show (a,b))
                                                Left xs -> Left $ "Invalid bytestring[" <> T.pack (show bs) <> "]: expected a double but returned " <> T.pack (show xs)
    fromPersistValue x = Left $ fromPersistValueError "Rational" "rational, double, string, integer, or bytestring" x

instance PersistField Bool where
    toPersistValue = PersistBool
    fromPersistValue (PersistBool b) = Right b
    fromPersistValue (PersistInt64 i) = Right $ i /= 0
    fromPersistValue (PersistByteString i) = case readInt i of
                                               Just (0,"") -> Right False
                                               Just (1,"") -> Right True
                                               xs -> Left $ T.pack $ "Failed to parse Haskell type `Bool` from PersistByteString. Original value:" ++ show i ++ ". Parsed by `readInt` as " ++ (show xs) ++ ". Expected '1'."
    fromPersistValue x = Left $ fromPersistValueError "Bool" "boolean, integer, or bytestring of '1' or '0'" x

instance PersistField Day where
    toPersistValue = PersistDay
    fromPersistValue (PersistDay d) = Right d
    fromPersistValue (PersistInt64 i) = Right $ ModifiedJulianDay $ toInteger i
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ fromPersistValueParseError "Day" x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ fromPersistValueParseError "Day" x
    fromPersistValue x = Left $ fromPersistValueError "Day" "day, integer, string or bytestring" x

instance PersistField TimeOfDay where
    toPersistValue = PersistTimeOfDay
    fromPersistValue (PersistTimeOfDay d) = Right d
    fromPersistValue x@(PersistText t) =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ -> Left $ fromPersistValueParseError "TimeOfDay" x
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ fromPersistValueParseError "TimeOfDay" x
    fromPersistValue x = Left $ fromPersistValueError "TimeOfDay" "time, string, or bytestring" x

instance PersistField UTCTime where
    toPersistValue = PersistUTCTime
    fromPersistValue (PersistUTCTime d) = Right d
#ifdef HIGH_PRECISION_DATE
    fromPersistValue (PersistInt64 i)   = Right $ posixSecondsToUTCTime $ (/ (1000 * 1000 * 1000)) $ fromIntegral $ i
#endif
    fromPersistValue x@(PersistText t)  =
        case reads $ T.unpack t of
            (d, _):_ -> Right d
            _ ->
                case parse8601 $ T.unpack t of
                    Nothing -> Left $ fromPersistValueParseError "UTCTime" x
                    Just x' -> Right x'
      where
#if MIN_VERSION_time(1,5,0)
        parse8601 = parseTimeM True defaultTimeLocale "%FT%T%Q"
#else
        parse8601 = parseTime defaultTimeLocale "%FT%T%Q"
#endif
    fromPersistValue x@(PersistByteString s) =
        case reads $ unpack s of
            (d, _):_ -> Right d
            _ -> Left $ fromPersistValueParseError "UTCTime" x

    fromPersistValue x = Left $ fromPersistValueError "UTCTime" "time, integer, string, or bytestring" x

-- | Prior to @persistent-2.11.0@, we provided an instance of
-- 'PersistField' for the 'Natural' type. This was in error, because
-- 'Natural' represents an infinite value, and databases don't have
-- reasonable types for this.
--
-- The instance for 'Natural' used the 'Int64' underlying type, which will
-- cause underflow and overflow errors. This type has the exact same code
-- in the instances, and will work seamlessly.
--
-- A more appropriate type for this is the 'Word' series of types from
-- "Data.Word". These have a bounded size, are guaranteed to be
-- non-negative, and are quite efficient for the database to store.
--
-- @since 2.11.0
newtype OverflowNatural = OverflowNatural { unOverflowNatural :: Natural }
    deriving (Eq, Show, Ord)

instance
  TypeError
    ( 'Text "The instance of PersistField for the Natural type was removed."
    ':$$: 'Text "Please see the documentation for OverflowNatural if you want to "
    ':$$: 'Text "continue using the old behavior or want to see documentation on "
    ':$$: 'Text "why the instance was removed."
    ':$$: 'Text ""
    ':$$: 'Text "This error instance will be removed in a future release."
    )
  =>
    PersistField Natural
  where
    toPersistValue = undefined
    fromPersistValue = undefined

instance PersistField OverflowNatural where
  toPersistValue = (toPersistValue :: Int64 -> PersistValue) . fromIntegral . unOverflowNatural
  fromPersistValue x = case (fromPersistValue x :: Either Text Int64) of
    Left err -> Left $ T.replace "Int64" "OverflowNatural" err
    Right int -> Right $ OverflowNatural $ fromIntegral int -- TODO use bimap?

instance PersistField a => PersistField (Maybe a) where
    toPersistValue Nothing = PersistNull
    toPersistValue (Just a) = toPersistValue a
    fromPersistValue PersistNull = Right Nothing
    fromPersistValue x = Just <$> fromPersistValue x

instance {-# OVERLAPPABLE #-} PersistField a => PersistField [a] where
    toPersistValue = PersistList . fmap toPersistValue
    fromPersistValue (PersistList l) = fromPersistList l
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) = fromPersistList values
    -- avoid the need for a migration to fill in empty lists.
    -- also useful when Persistent is not the only one filling in the data
    fromPersistValue (PersistNull) = Right []
    fromPersistValue x = Left $ fromPersistValueError "List" "list, string, bytestring or null" x

instance PersistField a => PersistField (V.Vector a) where
  toPersistValue = toPersistValue . V.toList
  fromPersistValue = either (\e -> Left ("Failed to parse Haskell type `Vector`: " `T.append` e))
                            (Right . V.fromList) . fromPersistValue

instance (Ord a, PersistField a) => PersistField (S.Set a) where
    toPersistValue = PersistList . fmap toPersistValue . S.toList
    fromPersistValue (PersistList list) =
      S.fromList <$> fromPersistList list
    fromPersistValue (PersistText t) = fromPersistValue (PersistByteString $ TE.encodeUtf8 t)
    fromPersistValue (PersistByteString bs)
        | Just values <- A.decode' (L.fromChunks [bs]) =
            S.fromList <$> fromPersistList values
    fromPersistValue PersistNull = Right S.empty
    fromPersistValue x = Left $ fromPersistValueError "Set" "list, string, bytestring or null" x

instance (PersistField a, PersistField b) => PersistField (a,b) where
    toPersistValue (x,y) = PersistList [toPersistValue x, toPersistValue y]
    fromPersistValue v =
        case fromPersistValue v of
            Right [x,y]  -> (,) <$> fromPersistValue x <*> fromPersistValue y
            Left e       -> Left e
            _            -> Left $ T.pack $ "Expected 2 item PersistList, received: " ++ show v

instance PersistField v => PersistField (IM.IntMap v) where
    toPersistValue = toPersistValue . IM.toList
    fromPersistValue = fmap IM.fromList . fromPersistValue

instance PersistField v => PersistField (M.Map T.Text v) where
    toPersistValue = PersistMap . fmap (second toPersistValue) . M.toList
    fromPersistValue = fromPersistMap <=< getPersistMap

instance PersistField PersistValue where
    toPersistValue = id
    fromPersistValue = Right

fromPersistList :: PersistField a => [PersistValue] -> Either T.Text [a]
fromPersistList = mapM fromPersistValue

fromPersistMap :: PersistField v
               => [(T.Text, PersistValue)]
               -> Either T.Text (M.Map T.Text v)
fromPersistMap = foldShortLeft fromPersistValue [] where
    -- a fold that short-circuits on Left.
    foldShortLeft f = go
      where
        go acc [] = Right $ M.fromList acc
        go acc ((k, v):kvs) =
          case f v of
            Left e   -> Left e
            Right v' -> go ((k,v'):acc) kvs

-- | FIXME Add documentation to that.
getPersistMap :: PersistValue -> Either T.Text [(T.Text, PersistValue)]
getPersistMap (PersistMap kvs) = Right kvs
getPersistMap (PersistText t)  = getPersistMap (PersistByteString $ TE.encodeUtf8 t)
getPersistMap (PersistByteString bs)
    | Just pairs <- A.decode' (L.fromChunks [bs]) = Right pairs
getPersistMap PersistNull = Right []
getPersistMap x = Left $ fromPersistValueError "[(Text, PersistValue)]" "map, string, bytestring or null" x

data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either Text Text)

instance PersistField Checkmark where
    toPersistValue Active   = PersistBool True
    toPersistValue Inactive = PersistNull
    fromPersistValue PersistNull         = Right Inactive
    fromPersistValue (PersistBool True)  = Right Active
    fromPersistValue (PersistInt64 1)    = Right Active
    fromPersistValue (PersistByteString i) = case readInt i of
                                               Just (0,"") -> Left "Failed to parse Haskell type `Checkmark`: found `0`, expected `1` or NULL"
                                               Just (1,"") -> Right Active
                                               xs -> Left $ T.pack $ "Failed to parse Haskell type `Checkmark` from PersistByteString. Original value:" ++ show i ++ ". Parsed by `readInt` as " ++ (show xs) ++ ". Expected '1'."
    fromPersistValue (PersistBool False) =
      Left $ T.pack "PersistField Checkmark: found unexpected FALSE value"
    fromPersistValue other =
      Left $ fromPersistValueError "Checkmark" "boolean, integer, bytestring or null" other


fromPersistValueError :: Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                      -> Text -- ^ Database type(s), should appear different from Haskell name, e.g. "integer" or "INT", not "Int".
                      -> PersistValue -- ^ Incorrect value
                      -> Text -- ^ Error message
fromPersistValueError haskellType databaseType received = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`; expected "
    , databaseType
    , " from database, but received: "
    , T.pack (show received)
    , ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

fromPersistValueParseError :: (Show a)
                           => Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                           -> a -- ^ Received value
                           -> Text -- ^ Error message
fromPersistValueParseError haskellType received = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`, but received "
    , T.pack (show received)
    ]
