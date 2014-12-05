{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Store 
    ( execRedisT
    , RedisBackend
    )where

import Database.Persist
import Control.Monad.IO.Class (MonadIO (..))
import qualified Database.Persist.Sql as Sql
import qualified Database.Redis as R
import qualified Data.ByteString as B
import Data.Text (Text, pack)
import Database.Persist.Redis.Config (RedisT, thisConnection)
import Database.Persist.Redis.Internal
import Web.PathPieces (PathPiece (..))

import Data.Aeson(FromJSON(..), ToJSON(..))

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
execRedisT :: (Monad m, MonadIO m) => R.RedisTx (R.Queued a) -> RedisT m a
execRedisT action = do
    conn <- thisConnection
    result <- liftIO $ R.runRedis conn $ R.multiExec action -- this is the question if we should support transaction here
    let r = desugar result
    case r of
        (Right x) -> return x
        (Left x)  -> fail x

instance PersistStore R.Connection where
    newtype BackendKey R.Connection = RedisKey Text
        deriving (Show, Read, Eq, Ord, PersistField, FromJSON, ToJSON)

    insert val = do
        keyId <- execRedisT $ createKey val
        let textKey = toKeyText val keyId
        key <- toKey textKey
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
            0 -> fail "there is no such key!"
            1 -> return ()
            _ -> fail "there are a lot of such keys!"

    get k = do
        r <- execRedisT $ R.hgetall (unKey k)
        if null r
            then return Nothing
            else do
                Entity _ val <- mkEntity k r
                return $ Just val

    update _   []   = return ()
    update key upds = do
        let fields = updatesToFields upds
        _ <- execRedisT $ R.hmset (unKey key) fields
        return ()

updatesToFields :: PersistEntity val => [Update val] -> [(B.ByteString, B.ByteString)]
updatesToFields = map updateToOneField
    where
        updateToOneField (Update field v up) = undefined
        updateToOneField (BackendUpdate up)  = undefined

instance PathPiece (BackendKey RedisBackend) where
    toPathPiece (RedisKey txt) = txt
    fromPathPiece txt = Just $ RedisKey txt 
-- some checking that entity exists and it is in format of entityname_id is omitted

instance Sql.PersistFieldSql (BackendKey RedisBackend) where
    sqlType _ = Sql.SqlOther (pack "doesn't make much sense for Redis backend")

