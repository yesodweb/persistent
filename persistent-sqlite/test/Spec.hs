{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main
  ( main
  -- avoid warnings
  , TestId
  ) where

import Control.Monad.IO.Class  (liftIO)
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Database.Sqlite as Sqlite
import Test.Hspec
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
  time UTCTime
|]

asIO :: IO a -> IO a
asIO = id

main :: IO ()
main = hspec $ do
    it "issue #328" $ asIO $ runSqlite (mkSqliteConnectionInfo ":memory:") $ do
        runMigration migrateAll
        _ <- insert . Test $ read "2014-11-30 05:15:25.123"
        [Single x] <- rawSql "select strftime('%s%f',time) from test" []
        liftIO $ x `shouldBe` Just ("141732452525.123" :: String)
    it "issue #339" $ asIO $ runSqlite (mkSqliteConnectionInfo ":memory:") $ do
        runMigration migrateAll
        now <- liftIO getCurrentTime
        tid <- insert $ Test now
        Just (Test now') <- get tid
        liftIO $ now' `shouldBe` now
    it "issue #564" $ asIO $ withSystemTempFile "test564.sqlite3"$ \fp h -> do
        hClose h
        conn <- Sqlite.open (T.pack fp)
        Sqlite.close conn
        return ()
    it "issue #527" $ asIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        insertMany_ $ replicate 1000 (Test $ read "2014-11-30 05:15:25.123")
