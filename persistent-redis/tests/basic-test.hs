{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Main where

import qualified Database.Redis as R
import Database.Persist
import Database.Persist.Redis
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)

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

mkKey :: (Monad m, PersistEntity val) => Text -> m (Key val)
mkKey s = case keyFromValues [PersistText s] of
    Right z -> return z
    Left  a -> fail (unpack a)

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