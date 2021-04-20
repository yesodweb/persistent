{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module UpsertWhere where

import Data.List              (sort)

import Database.Persist.Postgresql
import PgInit

share [mkPersist sqlSettings, mkMigrate "upsertWhereMigrate"] [persistLowerCase|
  Item
     name        Text sqltype=varchar(80)
     description Text
     price       Double Maybe
     quantity    Int Maybe

     Primary name
     deriving Eq Show Ord

|]

specs :: Spec
specs = describe "UpsertWhere" $ do
  let item1 = Item "item1" "" (Just 3) Nothing
      item2 = Item "item2" "hello world" Nothing (Just 2)
      items = [item1, item2]

  describe "upsertWhere" $ do
    it "inserts appropriately" $ runConnAssert $ do
      deleteWhere ([] :: [Filter Item])
      upsertWhere item1 [ItemDescription =. "i am item 1"] []
      Just item <- get (ItemKey "item1")
      item @== item1
    it "performs only updates given if record already exists" $ runConnAssert $ do
      deleteWhere ([] :: [Filter Item])
      let newDescription = "I am a new description"
      insert_ item1
      upsertWhere
        (Item "item1" "i am inserted description" (Just 1) (Just 2))
        [ItemDescription =. newDescription] 
        []
      Just item <- get (ItemKey "item1")
      item @== item1 { itemDescription = newDescription }

  describe "upsertManyWhere" $ do
    it "inserts fresh records" $ runConnAssert $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      let newItem = Item "item3" "fresh" Nothing Nothing
      upsertManyWhere
        (newItem : items)
        [copyField ItemDescription]
        []
        []
      dbItems <- map entityVal <$> selectList [] []
      sort dbItems @== sort (newItem : items)
    it "updates existing records" $ runConnAssert $ do
      deleteWhere ([] :: [Filter Item])
      let postUpdate = map (\i -> i { itemQuantity = fmap (+1) (itemQuantity i) }) items
      insertMany_ items
      upsertManyWhere
        items
        []
        [ItemQuantity +=. Just 1]
        []
      dbItems <- sort . fmap entityVal <$> selectList [] []
      dbItems @== sort postUpdate
    it "only copies passing values" $ runConnAssert $ do
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      let newItems = map (\i -> i { itemQuantity = Just 0, itemPrice = fmap (*2) (itemPrice i) }) items
          postUpdate = map (\i -> i { itemPrice = fmap (*2) (itemPrice i) }) items
      upsertManyWhere
        newItems
        [ 
          copyUnlessEq ItemQuantity (Just 0)
        , copyField ItemPrice
        ]
        []
        []
      dbItems <- sort . fmap entityVal <$> selectList [] []
      dbItems @== sort postUpdate
    it "inserts without modifying existing records if no updates specified" $ runConnAssert $ do
      let newItem = Item "item3" "hi friends!" Nothing Nothing
      deleteWhere ([] :: [Filter Item])
      insertMany_ items
      upsertManyWhere
        (newItem : items)
        []
        []
        []
      dbItems <- sort . fmap entityVal <$> selectList [] []
      dbItems @== sort (newItem : items)
    it "inserts without modifying existing records if no updates specified and there's a filter with True condition" $
      runConnAssert $ do
        let newItem = Item "item3" "hi friends!" Nothing Nothing
        deleteWhere ([] :: [Filter Item])
        insertMany_ items
        upsertManyWhere
          (newItem : items)
          []
          []
          [ItemDescription ==. "hi friends!"]
        dbItems <- sort . fmap entityVal <$> selectList [] []
        dbItems @== sort (newItem : items)
    it "inserts without updating existing records if there are updates specified but there's a filter with a False condition" $
      runConnAssert $ do
        let newItem = Item "item3" "hi friends!" Nothing Nothing
        deleteWhere ([] :: [Filter Item])
        insertMany_ items
        upsertManyWhere
          (newItem : items)
          []
          [ItemQuantity +=. Just 1]
          [ItemDescription ==. "hi friends!"]
        dbItems <- sort . fmap entityVal <$> selectList [] []
        dbItems @== sort (newItem : items)
    it "inserts new records but does not update existing records if there are updates specified but the modification condition is False" $
      runConnAssert $ do
        let newItem = Item "item3" "hi friends!" Nothing Nothing
        deleteWhere ([] :: [Filter Item])
        insertMany_ items
        upsertManyWhere
          (newItem : items)
          []
          [ItemQuantity +=. Just 1]
          [excludeNotEqualToOriginal ItemDescription]
        dbItems <- sort . fmap entityVal <$> selectList [] []
        dbItems @== sort (newItem : items)
    it "inserts new records and updates existing records if there are updates specified and the modification condition is True (because it's empty)" $
      runConnAssert $ do
        let newItem = Item "item3" "hello world" Nothing Nothing
            postUpdate = map (\i -> i {itemQuantity = fmap (+ 1) (itemQuantity i)}) items
        deleteWhere ([] :: [Filter Item])
        insertMany_ items
        upsertManyWhere
          (newItem : items)
          []
          [ItemQuantity +=. Just 1]
          []
        dbItems <- sort . fmap entityVal <$> selectList [] []
        dbItems @== sort (newItem : postUpdate)
    it "inserts new records and updates existing records if there are updates specified and the modification filter condition is triggered" $
       runConnAssert $ do
        let newItem = Item "item3" "hi friends!" Nothing Nothing
            postUpdate = map (\i -> i {itemQuantity = fmap (+1) (itemQuantity i)}) items
        deleteWhere ([] :: [Filter Item])
        insertMany_ items
        upsertManyWhere
          (newItem : items)
          [ 
            copyUnlessEq ItemDescription "hi friends!"
          , copyField ItemPrice
          ]
          [ItemQuantity +=. Just 1]
          [ItemDescription !=. "bye friends!"]
        dbItems <- sort . fmap entityVal <$> selectList [] []
        dbItems @== sort (newItem : postUpdate)
    it "inserts an item and doesn't apply the update if the filter condition is triggered" $ 
      runConnAssert $ do
        let newItem = Item "item3" "hello world" Nothing Nothing 
        deleteWhere ([] :: [Filter Item])
        insertMany_ items
        upsertManyWhere
          (newItem : items)
          []
          [ItemQuantity +=. Just 1]
          [excludeNotEqualToOriginal ItemDescription]
        dbItems <- sort . fmap entityVal <$> selectList [] []
        dbItems @== sort (newItem : items)
