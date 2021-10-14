{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UpsertWhere where

import PgInit

import Data.Time
import Database.Persist.Postgresql

share [mkPersist sqlSettings, mkMigrate "upsertWhereMigrate"] [persistLowerCase|

Item
    name        Text sqltype=varchar(80)
    description Text
    price       Double Maybe
    quantity    Int Maybe

    UniqueName name
    deriving Eq Show Ord

ItemMigOnly
    name        Text
    price       Double
    quantity    Int

    UniqueNameMigOnly name

    createdAt UTCTime MigrationOnly default=CURRENT_TIMESTAMP

|]

wipe :: IO ()
wipe = runConnAssert $ do
    deleteWhere ([] :: [Filter Item])
    deleteWhere ([] :: [Filter ItemMigOnly])

itDb :: String -> SqlPersistT (LoggingT (ResourceT IO)) a -> SpecWith (Arg (IO ()))
itDb msg action = it msg $ runConnAssert $ void action

specs :: Spec
specs = describe "UpsertWhere" $ do
    let item1 = Item "item1" "" (Just 3) Nothing
        item2 = Item "item2" "hello world" Nothing (Just 2)
        items = [item1, item2]

    describe "upsertWhere" $ before_ wipe $ do
        itDb "inserts appropriately" $ do
            upsertWhere item1 [ItemDescription =. "i am item 1"] []
            Just item <- fmap entityVal <$> getBy (UniqueName "item1")
            item `shouldBe` item1
        itDb "performs only updates given if record already exists" $ do
            let newDescription = "I am a new description"
            insert_ item1
            upsertWhere
                (Item "item1" "i am an inserted description" (Just 1) (Just 2))
                [ItemDescription =. newDescription]
                []
            Just item <- fmap entityVal <$> getBy (UniqueName "item1")
            item `shouldBe` item1 { itemDescription = newDescription }

        itDb "inserts with MigrationOnly fields (#1330)" $ do
            upsertWhere
                (ItemMigOnly "foobar" 20 1)
                [ItemMigOnlyPrice +=. 2]
                []

    describe "upsertManyWhere" $ do
        itDb "inserts fresh records" $ do
            insertMany_ items
            let newItem = Item "item3" "fresh" Nothing Nothing
            upsertManyWhere
                (newItem : items)
                [copyField ItemDescription]
                []
                []
            dbItems <- map entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : items)
        itDb "updates existing records" $ do
            let
                postUpdate =
                    map (\i -> i { itemQuantity = fmap (+1) (itemQuantity i) }) items
            insertMany_ items
            upsertManyWhere
                items
                []
                [ItemQuantity +=. Just 1]
                []
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` postUpdate
        itDb "only copies passing values" $ do
            insertMany_ items
            let newItems = map (\i -> i { itemQuantity = Just 0, itemPrice = fmap (*2) (itemPrice i) }) items
                postUpdate = map (\i -> i { itemPrice = fmap (*2) (itemPrice i) }) items
            upsertManyWhere
                newItems
                [ copyUnlessEq ItemQuantity (Just 0)
                , copyField ItemPrice
                ]
                []
                []
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` postUpdate
        itDb "inserts without modifying existing records if no updates specified" $ do
            let newItem = Item "item3" "hi friends!" Nothing Nothing
            insertMany_ items
            upsertManyWhere
                (newItem : items)
                []
                []
                []
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : items)
        itDb "inserts without modifying existing records if no updates specified and there's a filter with True condition" $
          do
            let newItem = Item "item3" "hi friends!" Nothing Nothing
            insertMany_ items
            upsertManyWhere
              (newItem : items)
              []
              []
              [ItemDescription ==. "hi friends!"]
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : items)
        itDb "inserts without updating existing records if there are updates specified but there's a filter with a False condition" $
          do
            let newItem = Item "item3" "hi friends!" Nothing Nothing
            insertMany_ items
            upsertManyWhere
              (newItem : items)
              []
              [ItemQuantity +=. Just 1]
              [ItemDescription ==. "hi friends!"]
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : items)
        itDb "inserts new records but does not update existing records if there are updates specified but the modification condition is False" $
          do
            let newItem = Item "item3" "hi friends!" Nothing Nothing
            insertMany_ items
            upsertManyWhere
              (newItem : items)
              []
              [ItemQuantity +=. Just 1]
              [excludeNotEqualToOriginal ItemDescription]
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : items)
        itDb "inserts new records and updates existing records if there are updates specified and the modification condition is True (because it's empty)" $
          do
            let newItem = Item "item3" "hello world" Nothing Nothing
                postUpdate = map (\i -> i {itemQuantity = fmap (+ 1) (itemQuantity i)}) items
            insertMany_ items
            upsertManyWhere
              (newItem : items)
              []
              [ItemQuantity +=. Just 1]
              []
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : postUpdate)
        itDb "inserts new records and updates existing records if there are updates specified and the modification filter condition is triggered" $
           do
            let newItem = Item "item3" "hi friends!" Nothing Nothing
                postUpdate = map (\i -> i {itemQuantity = fmap (+1) (itemQuantity i)}) items
            insertMany_ items
            upsertManyWhere
              (newItem : items)
              [
                copyUnlessEq ItemDescription "hi friends!"
              , copyField ItemPrice
              ]
              [ItemQuantity +=. Just 1]
              [ItemDescription !=. "bye friends!"]
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : postUpdate)
        itDb "inserts an item and doesn't apply the update if the filter condition is triggered" $
          do
            let newItem = Item "item3" "hello world" Nothing Nothing
            insertMany_ items
            upsertManyWhere
              (newItem : items)
              []
              [ItemQuantity +=. Just 1]
              [excludeNotEqualToOriginal ItemDescription]
            dbItems <- fmap entityVal <$> selectList [] []
            dbItems `shouldMatchList` (newItem : items)
