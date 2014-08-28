{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Config
    ( RedisAuth (..)
    , RedisConf (..)
    , R.RedisCtx
    , R.Redis
    , R.Connection
    , R.PortID (..)
    , RedisT (..)
    , runRedisPool
    , withRedisConn
    , thisConnection
    , module Database.Persist
    ) where

import Database.Persist
import qualified Database.Redis as R
import Data.Text (Text, unpack, pack)
import Data.Aeson (Value (Object, Number, String), (.:?), (.!=), FromJSON(..))
import Control.Monad (mzero, MonadPlus(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Applicative (Applicative (..))
import Control.Monad.Reader(ReaderT(..))
import Control.Monad.Reader.Class
import qualified Data.ByteString.Char8 as B
#if MIN_VERSION_aeson(0, 7, 0)
import Data.Scientific() -- we require only RealFrac instance of Scientific
#else
import Data.Attoparsec.Number
#endif

newtype RedisAuth =  RedisAuth Text deriving (Eq, Show)

-- | Information required to connect to a Redis server
data RedisConf = RedisConf {
    rdHost    :: Text,  -- ^ Host
    rdPort    :: R.PortID, -- ^ Port
    rdAuth    :: Maybe RedisAuth, -- ^ Auth info
    rdMaxConn :: Int -- ^ Maximum number of connections
} deriving (Show)

instance FromJSON R.PortID where
    parseJSON (Number x) = (return . R.PortNumber . fromInteger . truncate) x
    parseJSON _ = fail "couldn't parse port number"

instance FromJSON RedisAuth where
    parseJSON (String t) = (return . RedisAuth) t
    parseJSON _ = fail "couldn't parse auth"

-- | Monad reader transformer keeping Redis connection through out the work
type RedisT = ReaderT R.Connection

-- | Extracts connection from RedisT monad transformer
thisConnection :: Monad m => RedisT m R.Connection
thisConnection = ask

-- | Run a connection reader function against a Redis configuration
withRedisConn :: (Monad m, MonadIO m) => RedisConf -> (R.Connection -> m a) -> m a
withRedisConn conf connectionReader = do
    conn <- liftIO $ createPoolConfig conf
    connectionReader conn

runRedisPool :: RedisT m a -> R.Connection -> m a
runRedisPool r = runReaderT r

instance PersistConfig RedisConf where
    type PersistConfigBackend RedisConf = RedisT
    type PersistConfigPool RedisConf = R.Connection

    loadConfig (Object o) = do
        host               <- o .:? "host" .!= R.connectHost R.defaultConnectInfo
        port               <- o .:? "port" .!= R.connectPort R.defaultConnectInfo
        mPass              <- o .:? "password"
        maxConn            <- o .:? "maxConn" .!= R.connectMaxConnections R.defaultConnectInfo

        return RedisConf {
            rdHost = pack host,
            rdPort = port,
            rdAuth = mPass,
            rdMaxConn = maxConn
        }

    loadConfig _ = mzero

    createPoolConfig (RedisConf h p Nothing m) = 
        R.connect $
        R.defaultConnectInfo {
            R.connectHost = unpack h,
            R.connectPort = p,
            R.connectMaxConnections = m
        }
    createPoolConfig (RedisConf h p (Just (RedisAuth pwd)) m) = 
        R.connect $
        R.defaultConnectInfo {
            R.connectHost = unpack h,
            R.connectPort = p,
            R.connectAuth = Just $ B.pack $ unpack pwd,
            R.connectMaxConnections = m
        }

    runPool _ = runRedisPool
