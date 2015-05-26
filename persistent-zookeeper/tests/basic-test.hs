{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Database.Zookeeper ()
import Database.Persist
import Database.Persist.Zookeeper
import Database.Persist.Zookeeper.Internal
import Database.Persist.TH
import Language.Haskell.TH.Syntax ()
import Data.Int (Int64)
import Data.Maybe
import Data.Pool ()
import Test.Hspec

let zookeeperSettings = defaultZookeeperSettings
 in share [mkPersist zookeeperSettings] [persistLowerCase|
Person
    name String
    age Int
    hoge Int Maybe
    PersonU name
    deriving Show
    deriving Eq
|]

zookeeperConf :: ZookeeperConf
zookeeperConf = ZookeeperConf "localhost:2181" 10000 1 50 30

main :: IO ()
main =
  withZookeeperPool zookeeperConf $ \conn -> do
    hspec $ do
      let key = txtToKey "WyJzVGVzdC9ob2dlIl0="
      let val = Person "Test/hoge" 12 Nothing
      describe "PersistUnique test" $ do
        it "insertUnique" $ do
          v <- flip runZookeeperPool conn $ do
            deleteBy $ PersonU "Test/hoge"
            insertUnique val
          v `shouldBe` (Just key)
        it "getBy" $ do
          v <- flip runZookeeperPool conn $ do
            getBy $ PersonU "Test/hoge"
          (entityKey (fromJust v)) `shouldBe` key
          (entityVal (fromJust v)) `shouldBe` val
          v `shouldBe` (Just (Entity key val))
        it "deleteBy" $ do
          v <- flip runZookeeperPool conn $ do
            deleteBy $ PersonU "Test/hoge"
            getBy $ PersonU "Test/hoge"
          v `shouldBe` Nothing
      describe "PersistStore test" $ do
        it "StoreTest" $ do
          key' <- flip runZookeeperPool conn $ do
            insert val
          v <- flip runZookeeperPool conn $ do
            get key'
          print $ show key'
          v `shouldBe` (Just val)
          v' <- flip runZookeeperPool conn $ do
            delete key'
            get key'
          v' `shouldBe` Nothing
      describe "PersistQeuery test" $ do
        let check val' filter' expbool = case filterClause val' filter' of
                                           a@(bool,_,_) -> do
                                           print $ show a
                                           bool `shouldBe` expbool
        it "FilterTest Filter OR/AND" $ do
          check (Person "Test/hoge" 12 Nothing) [FilterOr[]] False
          check (Person "Test/hoge" 12 Nothing) [FilterAnd[]] True
        it "FilterTest Nothing" $ do
          check (Person "Test/hoge" 12 Nothing) [] True
          check (Person "Test/hoge" 12 (Just 3)) [] True
          check (Person "Test/hoge" 12 Nothing) [] True
        it "FilterTestEq" $ do
          check (Person "Test/hoge" 12 Nothing) [PersonName ==. ""] False
          check (Person "Test/hoge" 12 (Just 3)) [PersonHoge ==. Just 3] True
          check (Person "Test/hoge" 12 Nothing) [PersonAge ==. 12]  True
        it "FilterTestNe" $ do
          check (Person "Test/hoge" 12 Nothing) [PersonAge !=. 12] False
          check (Person "Test/hoge" 12 Nothing) [PersonAge !=. 11] True
          check (Person "Test/hoge" 12 (Just 4)) [PersonHoge !=. Just 4] False
          check (Person "Test/hoge" 12 Nothing) [PersonHoge !=. Just 3] True
          check (Person "Test/hoge" 12 (Just 4)) [PersonHoge !=. Just 3] True
        it "FilterTestLt" $ do
          check (Person "Test/hoge" 12 (Just 4)) [PersonHoge <=. Just 3] False
          check (Person "Test/hoge" 12 (Just 2)) [PersonHoge <=. Just 3] True
        it "StoreTest" $ do
          va <- flip runZookeeperPool conn $ do
            deleteWhere [PersonName !=. ""]
            _ <- insert (Person "hoge0" 1 Nothing)
            _ <- insert (Person "hoge1" 2 Nothing)
            _ <- insert (Person "hoge2" 3 Nothing)
            _ <- insert (Person "hoge3" 4 Nothing)
            selectList [PersonAge ==. 2] []
          (entityVal (head va)) `shouldBe` (Person "hoge1" 2 Nothing)
          [Entity _k v] <- flip runZookeeperPool conn $ do
            selectList [PersonName ==. "hoge2"] []
          v `shouldBe` (Person "hoge2" 3 Nothing)
          [Entity _k v1] <- flip runZookeeperPool conn $ do
            updateWhere [PersonName ==. "hoge2"] [PersonAge =. 10]
            selectList [PersonName ==. "hoge2"] []
          v1 `shouldBe` (Person "hoge2" 10 Nothing)
          v2 <- flip runZookeeperPool conn $ do
            selectList [PersonName !=. ""] []
          length v2 `shouldBe` 4
          v3 <- flip runZookeeperPool conn $ do
            deleteWhere [PersonName !=. ""]
            selectList [PersonName !=. ""] []
          length v3 `shouldBe` 0
