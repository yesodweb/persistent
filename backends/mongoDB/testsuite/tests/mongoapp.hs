{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
import Yesod
import Database.Persist
import Database.Persist.MongoDB
import Database.MongoDB.Connection -- (ConnPool)
import qualified Database.MongoDB as DB
import Control.Monad.Context (Context (..))
import Network.Abstract (ANetwork)
import Control.Monad.Trans.Reader
import Control.Monad.Util

mkPersist [persist|
Person
    name String Eq
    age Int
|]

-- runMongo :: MongoDBReader Database.MongoDB.Connection.Host (GGHandler M M IO) a -> GHandler M M a
runMongo :: MongoDBReader (GGHandler M M IO) a -> GHandler M M a
runMongo x = liftIOHandler $ 
  withMongoDBConn (DB.Database "test") "127.0.0.1" $ runMongoDBConn x DB.safe DB.Master

getConnection :: (ConnPool t, HostName)
getConnection = undefined

-- someAction :: Monad m => MongoDBReader DB.Host -> m (Key Person)
someAction = do
  insert $ Person "Bob" 20
  selectList [PersonNameEq "Bob"] [] 0 0

data M = M
instance Yesod M where approot _ = ""

mkYesod "M" [parseRoutes|/ RootR GET|]

getRootR :: GHandler M M RepHtml
getRootR = do
    people <- runMongo someAction
    defaultLayout [hamlet|<h1>HELLO #{personName $ snd $ head people}|]

main :: IO ()
main = warpDebug 3000 M
