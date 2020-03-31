module Database.Persist.Redis.Internal
    ( toKey
    , unKey
    , mkEntity
    , toKeyId
    , toKeyText
    , toInsertFields
    , toB
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Control.Monad.Fail (MonadFail)

import Database.Persist.Class
import Database.Persist.Types
import Database.Persist.Redis.Parser

toLabel :: FieldDef -> B.ByteString
toLabel = U.fromString . unpack . unDBName . fieldDB

toEntityString :: PersistEntity val => val -> Text
toEntityString = unDBName . entityDB . entityDef . Just

toEntityName :: EntityDef -> B.ByteString
toEntityName = U.fromString . unpack . unDBName . entityDB

mkEntity :: (MonadFail m, PersistEntity val) => Key val -> [(B.ByteString, B.ByteString)] -> m (Entity val)
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
toKeyText val k = toEntityString val `T.append` T.pack "_" `T.append` T.pack (show k)

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

toKey :: (Monad m, MonadFail m, PersistEntity val) => Text -> m (Key val)
toKey x = case q of
        Right z -> return z
        Left a -> fail (unpack a)
    where
        q  = keyFromValues [PersistText x]
