{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Store 
    ( RedisBackend
    , execRedisT
    )where

import Database.Persist
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Database.Redis as R
import Data.Text (Text)
import Database.Persist.Redis.Config (RedisT(..), thisConnection)
import Database.Persist.Redis.Internal

data RedisBackend

toOid :: (PersistEntity val) => Text -> Key val
toOid = Key . PersistText

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

instance (Applicative m, Functor m, MonadIO m, MonadBaseControl IO m) => PersistStore (RedisT m) where
    type PersistMonadBackend (RedisT m) = RedisBackend

    insert val = do
        keyId <- execRedisT $ createKey val
        let key    = toOid $ toKeyText val keyId
        _ <- insertKey key val
        return key

    insertKey (Key (PersistText key)) val = do
        let fields = toInsertFields val
        -- Inserts a hash map into <object>_<id> record
        _ <- execRedisT $ R.hmset (toB key) fields
        return ()
    insertKey _ _ = fail "Wrong key type in insertKey"

    repsert k@(Key (PersistText key)) val = do
        _ <- execRedisT $ R.del [toB key]
        insertKey k val
        return ()
    repsert _ _ = fail "Wrong key type in repsert"

    replace k val = do
        delete k
        insertKey k val
        return ()

    delete (Key (PersistText key)) = do
        r <- execRedisT $ R.del [toB key]
        case r of
            0 -> fail "there is no such key!"
            1 -> return ()
            _ -> fail "there are a lot of such keys!"
    delete _ = fail "Wrong key type in delete"

    get k@(Key (PersistText key)) = do
        r <- execRedisT $ R.hgetall (toB key)
        if null r
            then return Nothing
            else do
                Entity _ val <- mkEntity k r
                return $ Just val
    get  _ = fail "Wrong key type in get"
