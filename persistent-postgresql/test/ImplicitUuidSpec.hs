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

import Database.Persist.Postgresql

import Database.Persist.ImplicitIdDef

do
    let
        uuidDef =
           mkImplicitIdDefTypeable @UUID "uuid_generate_v1mc()"
        settings =
            setImplicitIdDef uuidDef sqlSettings
    share
        [mkPersist settings, mkMigrate "implicitUuidMigrate"] [persistLowerCase|

WithDefUuid
    name        Text sqltype=varchar(80)

    deriving Eq Show Ord

        |]

wipe :: IO ()
wipe = runConnAssert $ do
    deleteWhere ([] :: [Filter WithDefUuid])

itDb :: String -> SqlPersistT (LoggingT (ResourceT IO)) a -> SpecWith (Arg (IO ()))
itDb msg action = it msg $ runConnAssert $ void action

pass :: IO ()
pass = pure ()

specs :: Spec
specs = describe "ImplicitUuidSpec" $ before_ wipe $ do
    describe "WithDefUuidKey" $ do
        it "works on UUIDs" $ do
            let withDefUuidKey = WithDefUuidKey (UUID "Hello")
            pass
    describe "insert" $ do
        itDb "successfully has a default" $ do
            let matt = WithDefUuid
                    { withDefUuidName =
                        "Matt"
                    }
            k <- insert matt
            mrec <- get k
            mrec `shouldBe` Just matt


