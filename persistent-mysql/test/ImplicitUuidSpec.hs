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

module ImplicitUuidSpec where

import MyInit

import Data.Proxy
import Database.Persist.MySQL

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)

share
    [ mkPersist (sqlSettingsUuid "UUID()")
    , mkEntityDefList "entities"
    ]
    [persistLowerCase|

WithDefUuid
    name        Text sqltype=varchar(80)

    deriving Eq Show Ord

|]

implicitUuidMigrate :: Migration
implicitUuidMigrate = do
    migrateModels entities

wipe :: IO ()
wipe = db $ do
    rawExecute "DROP TABLE IF EXISTS with_def_uuid;" []
    runMigration implicitUuidMigrate

itDb :: String -> SqlPersistT (LoggingT (ResourceT IO)) a -> SpecWith (Arg (IO ()))
itDb msg action = it msg $ db $ void action

pass :: IO ()
pass = pure ()

spec :: Spec
spec = fdescribe "ImplicitUuidSpec" $ before_ wipe $ do
    describe "WithDefUuidKey" $ do
        it "works on UUIDs" $ do
            let withDefUuidKey = WithDefUuidKey (UUID "Hello")
            pass
    describe "getEntityId" $ do
        let idField = getEntityId (entityDef (Proxy @WithDefUuid))
        it "has a UUID SqlType" $ asIO $ do
            fieldSqlType idField `shouldBe` SqlOther "UUID"
        it "has a UUID type" $ asIO $ do
            fieldType idField `shouldBe` fieldTypeFromTypeable @UUID
        it "is an implicit ID column" $ asIO $ do
            fieldIsImplicitIdColumn idField `shouldBe` True

    describe "insert" $ do
        itDb "successfully has a default" $ do
            let matt = WithDefUuid
                    { withDefUuidName =
                        "Matt"
                    }
            k <- insert matt
            mrec <- get k
            liftIO $ mrec `shouldBe` Just matt

