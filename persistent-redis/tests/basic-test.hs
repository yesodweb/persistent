{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
module Main where

import qualified Database.Redis as R
import Database.Persist
import Database.Persist.Redis
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)

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

main :: IO ()
main = 
    withRedisConn redisConf $ runRedisPool $ do
        s <- insert $ Person "Test" 12
        liftIO $ print s
        let key = Key (PersistText "person_test")
        insertKey key $ Person "Test2" 45
        repsert s (Person "Test3" 55)
        g <- get key :: RedisT IO (Maybe Person)
        liftIO $ print g
        delete s
        return ()