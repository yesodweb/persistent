{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack, unpack)
import qualified Database.Redis as R
import Language.Haskell.TH.Syntax

import Database.Persist
import Database.Persist.Redis
import Database.Persist.TH

let redisSettings = mkPersistSettings (ConT ''RedisBackend)
 in share [mkPersist redisSettings] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

d :: R.ConnectInfo
d = R.defaultConnectInfo

host :: Text
host = pack $ R.connectHost d

redisConf :: RedisConf
redisConf = RedisConf host (R.connectPort d) Nothing 10

mkKey :: (MonadIO m, PersistEntity val) => Text -> m (Key val)
mkKey s = case keyFromValues [PersistText s] of
    Right z -> return z
    Left  a -> liftIO $ fail (unpack a)

main :: IO ()
main =
    withRedisConn redisConf $ runRedisPool $ do
        _ <- liftIO $ print "Inserting..."
        s <- insert $ Person "Test" 12
        _ <- liftIO $ print ("Received the key" ++ show s)
        key <- mkKey (pack "person_test")
        insertKey key $ Person "Test2" 45
        repsert s (Person "Test3" 55)
        g <- get key :: RedisT IO (Maybe Person)
        liftIO $ print g
        delete s
        return ()
