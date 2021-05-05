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

import PgInit

import Data.Proxy
import Database.Persist.Postgresql

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)

share
    [ mkPersist (sqlSettingsUuid "uuid_generate_v1mc()")
    , mkEntityDefList "entities"
    ]
    [persistLowerCase|

WithDefUuid
    name        Text sqltype=varchar(80)

    deriving Eq Show Ord

|]

implicitUuidMigrate :: Migration
implicitUuidMigrate = do
    runSqlCommand $ rawExecute "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\"" []
    migrateModels entities

wipe :: IO ()
wipe = runConnAssert $ do
    rawExecute "DROP TABLE with_def_uuid;" []
    runMigration implicitUuidMigrate

itDb :: String -> SqlPersistT (LoggingT (ResourceT IO)) a -> SpecWith (Arg (IO ()))
itDb msg action = it msg $ runConnAssert $ void action

pass :: IO ()
pass = pure ()

spec :: Spec
spec = describe "ImplicitUuidSpec" $ before_ wipe $ do
    describe "WithDefUuidKey" $ do
        it "works on UUIDs" $ do
            let withDefUuidKey = WithDefUuidKey (UUID "Hello")
            pass
    describe "getEntityId" $ do
        let Just idField = getEntityIdField (entityDef (Proxy @WithDefUuid))
        it "has a UUID SqlType" $ asIO $ do
            fieldSqlType idField `shouldBe` SqlOther "UUID"
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
            mrec `shouldBe` Just matt
