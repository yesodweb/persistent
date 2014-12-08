{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.IO.Class  (liftIO)
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import Test.Hspec

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
  time UTCTime
|]

asIO :: IO a -> IO a
asIO = id

main :: IO ()
main = hspec $ do
    it "issue #328" $ asIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        insert . Test $ read "2014-11-30 05:15:25.123"
        [Single x] <- rawSql "select strftime('%s%f',time) from test" []
        liftIO $ x `shouldBe` Just ("141732452525.123" :: String)
