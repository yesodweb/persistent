{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Redis.Internal
	( toInsertFields
    , toKeyId
    , toEntityName
    , toKeyText
    , toB
    , mkEntity
	) where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import Database.Persist.Types
import Database.Persist.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

toLabel :: FieldDef a -> B.ByteString
toLabel = U.fromString . unpack . unDBName . fieldDB

toEntityString :: PersistEntity val => val -> Text
toEntityString = unDBName . entityDB . entityDef . Just

toEntityName :: EntityDef a -> B.ByteString
toEntityName = U.fromString . unpack . unDBName . entityDB

toValue :: PersistValue -> B.ByteString
toValue (PersistText x) = U.fromString $ unpack x
toValue (PersistByteString x) = x
toValue (PersistInt64 x) = U.fromString $ show x
toValue (PersistDouble x) = U.fromString $ show x
toValue (PersistBool x) = U.fromString $ show x
toValue (PersistDay x) = U.fromString $ show x
toValue (PersistTimeOfDay x) = U.fromString $ show x
toValue (PersistUTCTime x) = U.fromString $ show x
toValue (PersistNull) = U.fromString ""
toValue (PersistList x) = U.fromString $ show x
toValue (PersistMap x) = U.fromString $ show x
toValue (PersistRational _) = undefined
toValue (PersistZonedTime _) = undefined
toValue (PersistObjectId _) = error "PersistObjectId is not supported."

castOne :: SqlType -> String -> PersistValue
castOne SqlString x = PersistText (T.pack x) 
castOne SqlInt32  x = PersistInt64 (read x)
castOne SqlInt64  x = PersistInt64 (read x)
castOne SqlBool   x = PersistBool (read x)
castOne SqlReal   x = PersistDouble (read x)
castOne _  _ = error "Unknown type"

redisToPerisistValues :: EntityDef SqlType -> [(B.ByteString, B.ByteString)] -> [PersistValue]
redisToPerisistValues entDef fields = recast fieldsAndValues
    where
        castColumns = map fieldSqlType (entityFields entDef)
        fieldsAndValues = zip castColumns (map (U.toString . snd) fields)
        recast :: [(SqlType, String)] -> [PersistValue]
        recast = map (uncurry castOne)

mkEntity :: (Monad m, PersistEntity val) => Key val -> EntityDef SqlType -> [(B.ByteString, B.ByteString)] -> m (Entity val)
mkEntity key entDef fields = do
    let values = redisToPerisistValues entDef fields
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