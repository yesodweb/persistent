{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Redis.Internal
	( toInsertFields
    , toKeyId
    , toEntityName
    , toKeyText
    , toB
    , mkEntity
    , unKey
    , toKey
	) where

import Control.Arrow((***))
import Data.Fixed
import Data.Time
import Data.Int (Int64)
import Data.Word (Word8)
import Control.Monad (liftM, liftM3)
import Data.Binary (Binary(..), encode, getWord8, Get)
import qualified Data.Binary as Q
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Database.Persist.Types
import Database.Persist.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U

toLabel :: FieldDef -> B.ByteString
toLabel = U.fromString . unpack . unDBName . fieldDB

toEntityString :: PersistEntity val => val -> Text
toEntityString = unDBName . entityDB . entityDef . Just

toEntityName :: EntityDef -> B.ByteString
toEntityName = U.fromString . unpack . unDBName . entityDB

newtype BinText = BinText { unBinText :: Text }
instance Binary BinText where
    put = put . U.fromString . unpack . unBinText
    get = do 
        str <- Q.get
        return $ BinText $ (T.pack . U.toString) str

newtype BinPico= BinPico { unBinPico :: Pico } 
instance Binary BinPico where
    put = put . toRational . unBinPico
    get = do
        x <- Q.get :: Get Rational
        return $ BinPico (fromRational x)

newtype BinDiffTime = BinDiffTime { unBinDiffTime :: DiffTime }
instance Binary BinDiffTime where
    put = put . toRational . unBinDiffTime
    get = do
        x <- Q.get :: Get Rational
        return $ BinDiffTime (fromRational x)

newtype BinDay = BinDay { unBinDay :: Day }
instance Binary BinDay where
    put (BinDay (ModifiedJulianDay x)) = put x
    get = do
        x <- Q.get :: Get Integer
        return $ BinDay (ModifiedJulianDay x)

newtype BinTimeOfDay = BinTimeOfDay { unBinTimeOfDay :: TimeOfDay }
instance Binary BinTimeOfDay where
    put (BinTimeOfDay (TimeOfDay h m s)) = do
        put h
        put m
        put (BinPico s)
    get = do
        let s = liftM unBinPico (Q.get :: Get BinPico)
        let tod = liftM3 TimeOfDay (Q.get :: Get Int) (Q.get :: Get Int) s
        liftM BinTimeOfDay tod

{-  
newtype BinZT = BinZT { unBinZT :: ZT }
instance Binary BinZT where
    put (BinZT (ZT (ZonedTime (LocalTime day timeOfDay) (TimeZone mins summer name)))) = do
        put (BinDay day)
        put (BinTimeOfDay timeOfDay)
        put mins
        put summer
        put name

    get = do
        day <- Q.get :: Get BinDay
        timeOfDay <- Q.get :: Get BinTimeOfDay
        mins <- Q.get :: Get Int
        summer <- Q.get :: Get Bool
        name <- Q.get :: Get String
        return $ BinZT $ ZT (ZonedTime (LocalTime (unBinDay day) (unBinTimeOfDay timeOfDay)) (TimeZone mins summer name))
-}
newtype BinPersistValue = BinPersistValue { unBinPersistValue :: PersistValue }
instance Binary BinPersistValue where
    put (BinPersistValue (PersistText x)) = do
        put (1 :: Word8)
        put $ (U.fromString . unpack) x

    put (BinPersistValue (PersistByteString x)) = do
        put (2 :: Word8)
        put x

    put (BinPersistValue (PersistInt64 x)) = do
        put (3 :: Word8)
        put x

    put (BinPersistValue (PersistDouble x)) = do
        put (4 :: Word8)
        put x

    put (BinPersistValue (PersistBool x)) = do 
        put (5 :: Word8)
        put x

    put (BinPersistValue (PersistDay day)) = do
        put (6 :: Word8)
        put (BinDay day)

    put (BinPersistValue (PersistTimeOfDay tod)) = do
        put (7 :: Word8)
        put (BinTimeOfDay tod)

    put (BinPersistValue (PersistUTCTime (UTCTime day pc))) = do
        put (8 :: Word8)
        put (BinDay day)
        put (BinDiffTime pc)

    put (BinPersistValue PersistNull) = put (9 :: Word8)
    put (BinPersistValue (PersistList x)) = do 
        put (10 :: Word8)
        put (map BinPersistValue x)

    put (BinPersistValue (PersistMap x)) = do
        put (11 :: Word8)
        put (map (BinText *** BinPersistValue) x)

    put (BinPersistValue (PersistRational x)) = do
        put (12 :: Word8)
        put x

    put (BinPersistValue (PersistDbSpecific _)) = undefined
    put (BinPersistValue (PersistObjectId _)) = error "PersistObjectId is not supported."

    get = do
        tag <- getWord8
        let pv = case tag of
                1 -> liftM (PersistText . unBinText) (Q.get :: Get BinText)
                2 -> liftM PersistByteString (Q.get :: Get B.ByteString)
                3 -> liftM PersistInt64 (Q.get :: Get Int64)
                4 -> liftM PersistDouble (Q.get :: Get Double)
                5 -> liftM PersistBool (Q.get :: Get Bool)
                6 -> liftM (PersistDay . unBinDay) (Q.get :: Get BinDay)
                7 -> liftM (PersistTimeOfDay . unBinTimeOfDay) (Q.get :: Get BinTimeOfDay)
                8 -> do
                    d <- Q.get :: Get BinDay
                    dt <- Q.get :: Get BinDiffTime
                    let utctime = UTCTime (unBinDay d) (unBinDiffTime dt)
                    return $  PersistUTCTime utctime
                9 -> return PersistNull
                10-> liftM (PersistList . map unBinPersistValue) (Q.get :: Get [BinPersistValue])
                11-> liftM (PersistMap . map (unBinText *** unBinPersistValue)) (Q.get :: Get [(BinText, BinPersistValue)])
                12-> liftM PersistRational (Q.get :: Get Rational)
--                13-> liftM (PersistZonedTime . unBinZT) (Q.get :: Get BinZT)
                z -> fail ("Incorrect tag " ++ show z ++ " came to Binary deserialization")
        liftM BinPersistValue pv

toValue :: PersistValue -> B.ByteString
toValue = L.toStrict . encode . BinPersistValue

castOne :: B.ByteString -> PersistValue
castOne = unBinPersistValue . Q.decode . L.fromStrict

redisToPerisistValues :: [(B.ByteString, B.ByteString)] -> [PersistValue]
redisToPerisistValues = map (castOne . snd)

mkEntity :: (Monad m, PersistEntity val) => Key val -> [(B.ByteString, B.ByteString)] -> m (Entity val)
mkEntity key fields = do
    let values = redisToPerisistValues fields
    let v = fromPersistValues values
    case v of
        Right body -> return $ Entity key body
        Left a -> fail (unpack a)


zipAndConvert :: PersistField t => [FieldDef] -> [t] -> [(B.ByteString, B.ByteString)]
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

unKey :: (PersistEntity val) => Key val -> B.ByteString
unKey = toValue . head . keyToValues

toKey :: (Monad m, PersistEntity val) => Text -> m (Key val)
toKey x = case q of 
        Right z -> return z
        Left a -> fail (unpack a)
    where
        q  = keyFromValues [PersistText x]
