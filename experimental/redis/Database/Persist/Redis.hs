{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
-- | A redis backend for persistent.
module Database.Persist.Redis
    ( RedisReader
    , runRedis
    , withRedis
    , Connection
    , Pool
    , module Database.Persist
    ) where

import Database.Persist
import Database.Persist.Base
import Database.Persist.Pool
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import qualified Database.Redis.Redis as R
import Control.Applicative (Applicative)
import Control.Monad (forM_, forM)
import Database.Redis.ByteStringClass
import qualified Data.ByteString.UTF8 as SU
import Data.Maybe (fromMaybe, mapMaybe)

-- FIXME make more intelligent
instance BS PersistValue where
    toBS = SU.fromString . show
    fromBS = read . SU.toString

type Connection = R.Redis

-- | A ReaderT monad transformer holding a sqlite database connection.
newtype RedisReader m a = RedisReader (ReaderT Connection m a)
    deriving (Monad, MonadIO, MonadTrans, MonadCatchIO, Functor,
              Applicative)

-- | Handles opening and closing of the database connection pool automatically.
withRedis :: MonadCatchIO m
           => String -- ^ hostname
           -> String -- ^ port
           -> Int -- ^ number of connections to open
           -> (Pool Connection -> m a) -> m a
withRedis host port i f = createPool (R.connect host port) R.disconnect i f

-- | Run a series of database actions. Remember, redis does not support
-- transactions, so nothing will be rolled back on exceptions.
runRedis :: MonadCatchIO m => RedisReader m a -> Pool Connection -> m a
runRedis (RedisReader r) pconn = withPool' pconn $ runReaderT r

dummyFromKey :: Key v -> v
dummyFromKey _ = error "dummyFromKey"

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

instance MonadIO m => PersistBackend (RedisReader m) where
    initialize _ = return ()
    insert val = do
        r <- RedisReader ask
        let t = entityDef val
        let name = entityName t
        R.RInt i <- liftIO $ R.incr r $ "global:" ++ name ++ ":nextId"
        let i' = toPersistKey $ fromIntegral i
        replace i' val
        return i'
    replace i' val = do
        let i = show $ fromPersistKey i'
        r <- RedisReader ask
        let t = entityDef val
        let name = entityName t
        let vals = map toPersistValue $ toPersistFields val
        let cols = map (\(x, _, _) -> x) $ entityColumns $ entityDef val
        liftIO $ forM_ (zip cols vals) $ \(col, val) ->
            R.set r (name ++ ":by-id:" ++ i ++ ":" ++ col) val
        liftIO $ R.sadd r (name ++ ":ids") i
        return ()
    get eid' = do
        r <- RedisReader ask
        let def = entityDef $ dummyFromKey eid'
        let name = entityName def
        let eid = show $ fromPersistKey eid'
        let cols = map (\(x, _, _) -> x) $ entityColumns def
        R.RInt exists <- liftIO $ R.sismember r (name ++ ":ids") eid
        if exists == 0
            then return Nothing
            else do
                let go s = do
                    R.RBulk x <- R.get r $ name ++ ":by-id:" ++ eid ++ ":" ++ s
                    return $ fromMaybe PersistNull x
                vals <- liftIO $ mapM go cols
                case fromPersistValues vals of
                    Left s -> error s
                    Right x -> return $ Just x
    select filts ords = do
        r <- RedisReader ask
        let def = entityDef $ dummyFromFilts filts
        let name = entityName def
        R.RMulti x <- liftIO $ R.smembers r $ name ++ ":ids"
        let go (R.RBulk (Just s)) = Just $ toPersistKey $ read s
            go _ = Nothing
        let ids = maybe [] (mapMaybe go) x
        forM ids $ \i -> do
            Just val <- get i
            return (i, val)
        -- FIXME apply filters and orders
