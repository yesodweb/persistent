{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans -O0 #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module InsertDuplicateUpdate where

import           Init
#ifdef WITH_MYSQL
import Database.Persist.MySQL
import Data.List (sort)

share [mkPersist sqlSettings, mkMigrate "duplicateMigrate"] [persistUpperCase|
  Item
     name        Text sqltype=varchar(80)
     description Text
     price       Double Maybe
     quantity    Int Maybe

     Primary name
     deriving Eq Show Ord

|]

specs :: Spec
specs = describe "DuplicateKeyUpdate" $ do
  let item1 = Item "item1" "" (Just 3) Nothing
      item2 = Item "item2" "hello world" Nothing (Just 2)
      items = [item1, item2]
  describe "insertOnDuplicateKeyUpdate" $ do
    it "inserts appropriately" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertOnDuplicateKeyUpdate item1 [ItemDescription =. "i am item 1"]
      Just item <- get (ItemKey "item1")
      item @== item1

    it "performs only updates given if record already exists" $ db $ do
      deleteWhere ([] :: [Filter Item])
      let newDescription = "I am a new description"
      _ <- insert item1
      insertOnDuplicateKeyUpdate
        (Item "item1" "i am inserted description" (Just 1) (Just 2))
        [ItemDescription =. newDescription]
      Just item <- get (ItemKey "item1")
      item @== item1 { itemDescription = newDescription }

  describe "insertManyOnDuplicateKeyUpdate" $ do
    it "inserts fresh records" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      let newItem = Item "item3" "fresh" Nothing Nothing
      insertManyOnDuplicateKeyUpdate
        (newItem : items)
        [SomeField ItemDescription]
        []
      dbItems <- map entityVal <$> selectList [] []
      sort dbItems @== sort (newItem : items)
    it "updates existing records" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      insertManyOnDuplicateKeyUpdate
        items
        []
        [ItemQuantity +=. Just 1]
    it "only copies passing values" $ db $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      let newItems = map (\i -> i { itemQuantity = Just 0, itemPrice = fmap (*2) (itemPrice i) }) items
          postUpdate = map (\i -> i { itemPrice = fmap (*2) (itemPrice i) }) items
      insertManyOnDuplicateKeyUpdate
        newItems
        [ copyUnlessEq ItemQuantity (Just 0)
        , SomeField ItemPrice
        ]
        []
      dbItems <- sort . fmap entityVal <$> selectList [] []
      dbItems @== sort postUpdate
    it "inserts without modifying existing records if no updates specified" $ db $ do
      let newItem = Item "item3" "hi friends!" Nothing Nothing
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      insertManyOnDuplicateKeyUpdate
        (newItem : items)
        []
        []
      dbItems <- sort . fmap entityVal <$> selectList [] []
      dbItems @== sort (newItem : items)

#else
specs :: Spec
specs = describe "DuplicateKeyUpdate" $ do
  it "Is only supported on MySQL currently." $ do
    True `shouldBe` True
#endif
