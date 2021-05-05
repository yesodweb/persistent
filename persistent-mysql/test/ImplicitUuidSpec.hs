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
    name        Text

    deriving Eq Show Ord

|]

implicitUuidMigrate :: Migration
implicitUuidMigrate = do
    migrateModels entities

wipe :: IO ()
wipe = db $ do
    rawExecute "DROP TABLE IF EXISTS with_def_uuid;" []
    void $ runMigrationSilent implicitUuidMigrate

itDb :: String -> SqlPersistT (LoggingT (ResourceT IO)) a -> SpecWith (Arg (IO ()))
itDb msg action = it msg $ db $ void action

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
        it "has a SqlString SqlType" $ asIO $ do
            fieldSqlType idField `shouldBe` SqlString
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
            uuids <- selectList @WithDefUuid [] []
            liftIO $ do
                -- MySQL's insert functionality is currently broken. The @k@
                -- here is derived from @SELECT LAST_INSERT_ID()@ which only
                -- works on auto incrementing IDs.
                --
                -- See #1251 for more details.
                mrec `shouldBe` Nothing

                map entityVal uuids `shouldSatisfy` (matt `elem`)
