{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Internal
	( toInsertFields
    , toKeyId
    , toEntityName
    , toKeyText
    , toB
    , mkEntity
	) where

import Data.Fixed
import Data.Time
import Data.Int (Int64)
import Data.Word (Word8)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary (Binary(..), encode, getWord8, Get)
import qualified Data.Binary as Q
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Database.Persist.Types
import Database.Persist.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U

toLabel :: FieldDef a -> B.ByteString
toLabel = U.fromString . unpack . unDBName . fieldDB

toEntityString :: PersistEntity val => val -> Text
toEntityString = unDBName . entityDB . entityDef . Just

toEntityName :: EntityDef a -> B.ByteString
toEntityName = U.fromString . unpack . unDBName . entityDB

instance Binary Text where
    put = put . U.fromString . unpack
    get = do 
        str <- Q.get
        return $ (T.pack . U.toString) str

instance HasResolution a => Binary (Fixed a) where
    put = put . toRational
    get = do
        x <- Q.get :: Get Rational
        return $ fromRational x

instance Binary DiffTime where
    put = put . toRational
    get = do
        x <- Q.get :: Get Rational
        return $ fromRational x

instance Binary Day where
    put (ModifiedJulianDay x) = put x
    get = do
        x <- Q.get :: Get Integer
        return $ ModifiedJulianDay x

instance Binary TimeOfDay where
    put (TimeOfDay h m s) = do
        put h
        put m
        put s
    get = liftM3 TimeOfDay (Q.get :: Get Int) (Q.get :: Get Int) (Q.get :: Get Pico)

instance Binary ZT where
    put (ZT (ZonedTime (LocalTime day timeOfDay) (TimeZone mins summer name))) = do
        put day
        put timeOfDay
        put mins
        put summer
        put name

    get = do
        day <- Q.get :: Get Day
        timeOfDay <- Q.get :: Get TimeOfDay
        mins <- Q.get :: Get Int
        summer <- Q.get :: Get Bool
        name <- Q.get :: Get String
        return $ ZT (ZonedTime (LocalTime day timeOfDay) (TimeZone mins summer name))

instance Binary PersistValue where
    put (PersistText x) = do
        put (1 :: Word8)
        put $ (U.fromString . unpack) x

    put (PersistByteString x) = do
        put (2 :: Word8)
        put x

    put (PersistInt64 x) = do
        put (3 :: Word8)
        put x

    put (PersistDouble x) = do
        put (4 :: Word8)
        put x

    put (PersistBool x) = do 
        put (5 :: Word8)
        put x

    put (PersistDay day) = do
        put (6 :: Word8)
        put day

    put (PersistTimeOfDay tod) = do
        put (7 :: Word8)
        put tod

    put (PersistUTCTime (UTCTime day pc)) = do
        put (8 :: Word8)
        put day
        put pc

    put (PersistNull) = put (9 :: Word8)
    put (PersistList x) = do 
        put (10 :: Word8)
        put x

    put (PersistMap x) = do
        put (11 :: Word8)
        put x

    put (PersistRational x) = do
        put (12 :: Word8)
        put x

    put (PersistZonedTime x) = do
        put (13 :: Word8)
        put x

    put (PersistDbSpecific _) = undefined
    put (PersistObjectId _) = error "PersistObjectId is not supported."

    get = do
        tag <- getWord8
        case tag of
            1 -> liftM PersistText (Q.get :: Get Text)
            2 -> liftM PersistByteString (Q.get :: Get B.ByteString)
            3 -> liftM PersistInt64 (Q.get :: Get Int64)
            4 -> liftM PersistDouble (Q.get :: Get Double)
            5 -> liftM PersistBool (Q.get :: Get Bool)
            6 -> liftM PersistDay (Q.get :: Get Day)
            7 -> liftM PersistTimeOfDay (Q.get :: Get TimeOfDay)
            8 -> liftM PersistUTCTime $ liftM2 UTCTime (Q.get :: Get Day) (Q.get :: Get DiffTime)
            9 -> return PersistNull
            10-> liftM PersistList (Q.get :: Get [PersistValue])
            11-> liftM PersistMap (Q.get :: Get [(Text, PersistValue)])
            12-> liftM PersistRational (Q.get :: Get Rational)
            13-> liftM PersistZonedTime (Q.get :: Get ZT)
            _ -> fail "Incorrect tag came to Binary deserialization"

toValue :: PersistValue -> B.ByteString
toValue = L.toStrict . encode

castOne :: B.ByteString -> PersistValue
castOne = Q.decode . L.fromStrict

redisToPerisistValues :: [(B.ByteString, B.ByteString)] -> [PersistValue]
redisToPerisistValues = map (castOne . snd)

mkEntity :: (Monad m, PersistEntity val) => Key val -> [(B.ByteString, B.ByteString)] -> m (Entity val)
mkEntity key fields = do
    let values = redisToPerisistValues fields
    let v = fromPersistValues values
    case v of
        Right body -> return $ Entity key body
        Left a -> fail (unpack a)


zipAndConvert :: PersistField t => [FieldDef a] -> [t] -> [(B.ByteString, B.ByteString)]
zipAndConvert [] _ = []
zipAndConvert _ [] = []
zipAndConvert (e:efields) (p:pfields) = 
    let pv = toPersistValue p
    in
        if pv == PersistNull then zipAndConvert efields pfields
            else (toLabel e, toValue pv) : zipAndConvert efields pfields

-- | Create a list for create/update in Redis store
toInsertFields :: PersistEntity val => val -> [(B.ByteString, B.ByteString)]
toInsertFields record = zipAndConvert entity fields
    where
        entity = entityFields $ entityDef $ Just record
        fields = toPersistFields record

underscoreBs :: B.ByteString
underscoreBs = U.fromString "_"

-- | Make a key for given entity and id
toKeyText :: PersistEntity val => val -> Integer -> Text
toKeyText val k = T.append (T.append (toEntityString val) "_") (T.pack $ show k)

toB :: Text -> B.ByteString
toB = U.fromString . unpack

-- | Create a string key for given entity
toObjectPrefix :: PersistEntity val => val -> B.ByteString
toObjectPrefix val = B.append (toEntityName $ entityDef $ Just val) underscoreBs

idBs :: B.ByteString
idBs = U.fromString "id"

-- | Construct an id key, that is incremented for access
toKeyId :: PersistEntity val => val -> B.ByteString
toKeyId val = B.append (toObjectPrefix val) idBs