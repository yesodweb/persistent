{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Zookeeper.Config(
  ZookeeperConf(..)
, Connection
, Action
, execZookeeper
, withZookeeperPool
, runZookeeperPool
, defaultZookeeperConf
, defaultZookeeperSettings
) where

import Database.Persist
import Database.Persist.TH
import Language.Haskell.TH
import qualified Database.Zookeeper as Z
import qualified Database.Zookeeper.Pool as Z
import Data.Pool (Pool, withResource)
import Data.Aeson
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader
import Data.Scientific() -- we require only RealFrac instance of Scientific
import Data.Time (NominalDiffTime)
import Control.Exception (throwIO)
import Control.Concurrent (threadDelay)

-- | Information required to connect to a Zookeeper server
data ZookeeperConf = ZookeeperConf {
    zCoord    :: String
  , zTimeout  :: Z.Timeout
  , zNumStripes :: Int
  , zIdleTime :: NominalDiffTime
  , zMaxResources :: Int
} deriving (Show)

type Connection = Pool Z.Zookeeper
type Action = ReaderT Z.Zookeeper

instance HasPersistBackend Z.Zookeeper Z.Zookeeper where
  persistBackend = id

execZookeeper :: (Read a,Show a,Monad m, MonadIO m) => (Z.Zookeeper -> IO (Either Z.ZKError a)) -> Action m a
execZookeeper action = do
  s <- ask
  liftIO $ waitConnectedState s
  r <- liftIO $ action s
  case r of
    (Right x) -> return x
    (Left x)  -> liftIO $ throwIO $ userError $ "Zookeeper error: code" ++ show x --fail $ show x
  where
    waitConnectedState zh = do
      s <- Z.getState zh
      case s of
        Z.ConnectingState -> do
          threadDelay (50*1000)
        _ -> return ()

-- | Run a connection reader function against a Zookeeper configuration
withZookeeperPool :: (Monad m, MonadIO m) => ZookeeperConf -> (Connection -> m a) -> m a
withZookeeperPool conf connectionReader = do
  conn <- liftIO $ createPoolConfig conf
  connectionReader conn

runZookeeperPool :: MonadBaseControl IO m =>
                    Action m b -> Connection -> m b
runZookeeperPool action pool = withResource pool (\stat -> runReaderT action stat)

defaultZookeeperConf :: ZookeeperConf
defaultZookeeperConf = ZookeeperConf "localhost:2181" 300000 1 300000 30

defaultZookeeperSettings :: MkPersistSettings
defaultZookeeperSettings = (mkPersistSettings $ ConT ''Z.Zookeeper)

instance PersistConfig ZookeeperConf where
  type PersistConfigBackend ZookeeperConf = Action
  type PersistConfigPool ZookeeperConf = Connection

  loadConfig (Object o) = do
      coord <- o .:? "coord" .!= "localhost:2181/"
      timeout <- o .:? "timeout" .!= 300000
      numstripes <- o .:? "num-stripes" .!= 1
      (idletime :: Int) <- o .:? "idletime" .!= 300000
      maxresources <- o .:? "max-resource" .!= 30

      return ZookeeperConf {
          zCoord = coord
        , zTimeout = timeout
        , zNumStripes = numstripes
        , zIdleTime = fromIntegral idletime
        , zMaxResources = maxresources
      }

  loadConfig _ = mzero

  createPoolConfig (ZookeeperConf h t s idle maxres ) =
      Z.connect h t Nothing Nothing s idle maxres

  runPool _ = runZookeeperPool
