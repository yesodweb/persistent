{-# LANGUAGE PatternSynonyms #-}

-- | This module contains an intermediate representation of values before the
-- backends serialize them into explicit database types.
--
-- @since 2.13.0.0
module Database.Persist.PersistValue
    ( module Database.Persist.PersistValue
    , PersistValue(.., PersistLiteral, PersistLiteralEscaped, PersistDbSpecific)
    ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Vector as V
import Data.Int (Int64)
import qualified Data.Scientific
import Data.Text.Encoding.Error (lenientDecode)
import Data.Bits (shiftL, shiftR)
import Control.Arrow (second)
import Numeric (readHex, showHex)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.ByteString (ByteString, foldl')
import Data.Time (Day, TimeOfDay, UTCTime)
import Web.PathPieces (PathPiece(..))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Web.HttpApiData
       ( FromHttpApiData(..)
       , ToHttpApiData(..)
       , parseUrlPieceMaybe
       , readTextData
       )

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
    | PersistObjectId ByteString
    -- ^ Intended especially for MongoDB backend
    | PersistArray [PersistValue]
    -- ^ Intended especially for PostgreSQL backend for text arrays
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
            Left  e -> error $ Text.unpack e
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
fromPersistValueText (PersistInt64 i) = Right $ Text.pack $ show i
fromPersistValueText (PersistDouble d) = Right $ Text.pack $ show d
fromPersistValueText (PersistRational r) = Right $ Text.pack $ show r
fromPersistValueText (PersistDay d) = Right $ Text.pack $ show d
fromPersistValueText (PersistTimeOfDay d) = Right $ Text.pack $ show d
fromPersistValueText (PersistUTCTime d) = Right $ Text.pack $ show d
fromPersistValueText PersistNull = Left "Unexpected null"
fromPersistValueText (PersistBool b) = Right $ Text.pack $ show b
fromPersistValueText (PersistList _) = Left "Cannot convert PersistList to Text"
fromPersistValueText (PersistMap _) = Left "Cannot convert PersistMap to Text"
fromPersistValueText (PersistObjectId _) = Left "Cannot convert PersistObjectId to Text"
fromPersistValueText (PersistArray _) = Left "Cannot convert PersistArray to Text"
fromPersistValueText (PersistLiteral_ _ _) = Left "Cannot convert PersistLiteral to Text"

instance A.ToJSON PersistValue where
    toJSON (PersistText t) = A.String $ Text.cons 's' t
    toJSON (PersistByteString b) = A.String $ Text.cons 'b' $ TE.decodeUtf8 $ B64.encode b
    toJSON (PersistInt64 i) = A.Number $ fromIntegral i
    toJSON (PersistDouble d) = A.Number $ Data.Scientific.fromFloatDigits d
    toJSON (PersistRational r) = A.String $ Text.pack $ 'r' : show r
    toJSON (PersistBool b) = A.Bool b
    toJSON (PersistTimeOfDay t) = A.String $ Text.pack $ 't' : show t
    toJSON (PersistUTCTime u) = A.String $ Text.pack $ 'u' : show u
    toJSON (PersistDay d) = A.String $ Text.pack $ 'd' : show d
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
            A.String $ Text.cons prefix encoded
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
        case Text.uncons t0 of
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
                $ headMay $ readHex $ Text.unpack t
            Just (c, _) -> fail $ "Unknown prefix: " ++ [c]
      where
        headMay []    = Nothing
        headMay (x:_) = Just x
        readMay t =
            case reads $ Text.unpack t of
                (x, _):_ -> return x
                [] -> fail "Could not read"

        -- taken from crypto-api
        -- |@i2bs bitLen i@ converts @i@ to a 'ByteString' of @bitLen@ bits (must be a multiple of 8).
        i2bs :: Int -> Integer -> ByteString
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

