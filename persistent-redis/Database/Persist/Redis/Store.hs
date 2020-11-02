{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Store
    ( execRedisT
    , RedisBackend
    )where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson(FromJSON(..), ToJSON(..))
import Data.Text (Text, pack)
import qualified Database.Redis as R
import Web.HttpApiData (ToHttpApiData (..), FromHttpApiData (..), parseUrlPieceMaybe)
import Web.PathPieces (PathPiece(..))

import Database.Persist
import Database.Persist.Redis.Config (RedisT, thisConnection)
import Database.Persist.Redis.Internal
import Database.Persist.Redis.Update
import qualified Database.Persist.Sql as Sql

type RedisBackend = R.Connection

-- | Fetches a next key from <object>_id record
createKey :: (R.RedisCtx m f, PersistEntity val) => val -> m (f Integer)
createKey val = do
    let keyId = toKeyId val
    R.incr keyId

desugar :: R.TxResult a -> Either String a
desugar (R.TxSuccess x) =  Right x
desugar R.TxAborted = Left "Transaction aborted!"
desugar (R.TxError string) = Left string

-- | Execute Redis transaction inside RedisT monad transformer
execRedisT :: (MonadIO m) => R.RedisTx (R.Queued a) -> RedisT m a
execRedisT action = do
    conn <- thisConnection
    result <- liftIO $ R.runRedis conn $ R.multiExec action -- this is the question if we should support transaction here
    let r = desugar result
    case r of
        (Right x) -> return x
        (Left x)  -> liftIO $ fail x

instance HasPersistBackend R.Connection where
  type BaseBackend R.Connection = R.Connection
  persistBackend = id

instance PersistCore R.Connection where
    newtype BackendKey R.Connection = RedisKey Text
        deriving (Show, Read, Eq, Ord, PersistField, FromJSON, ToJSON)

instance PersistStoreRead R.Connection where
    get k = do
        r <- execRedisT $ R.hgetall (unKey k)
        if null r
            then return Nothing
            else do
                Entity _ val <- liftIO $ mkEntity k r
                return $ Just val

instance PersistStoreWrite R.Connection where
    insert val = do
        keyId <- execRedisT $ createKey val
        let textKey = toKeyText val keyId
        key <- liftIO $ toKey textKey
        _ <- insertKey key val
        return key

    insertKey k val = do
        let fields = toInsertFields val
        -- Inserts a hash map into <object>_<id> record
        _ <- execRedisT $ R.hmset (unKey k) fields
        return ()

    repsert k val = do
        _ <- execRedisT $ R.del [unKey k]
        insertKey k val
        return ()

    replace k val = do
        delete k
        insertKey k val
        return ()

    delete k = do
        r <- execRedisT $ R.del [unKey k]
        case r of
            0 -> liftIO $ fail "there is no such key!"
            1 -> return ()
            _ -> liftIO $ fail "there are a lot of such keys!"

    update _ [] = return ()
    update k upds = do
        r <- execRedisT $ R.hgetall (unKey k)
        if null r
            then pure ()
            else do
                v <- liftIO $ mkEntity k r
                let (Entity _ val) = cmdUpdate v upds
                insertKey k val
        return()

instance ToHttpApiData (BackendKey RedisBackend) where
    toUrlPiece (RedisKey txt) = txt

instance FromHttpApiData (BackendKey RedisBackend) where
    parseUrlPiece = return . RedisKey
-- some checking that entity exists and it is in format of entityname_id is omitted

instance PathPiece (BackendKey RedisBackend) where
  toPathPiece   = toUrlPiece
  fromPathPiece = parseUrlPieceMaybe

instance Sql.PersistFieldSql (BackendKey RedisBackend) where
    sqlType _ = Sql.SqlOther (pack "doesn't make much sense for Redis backend")
