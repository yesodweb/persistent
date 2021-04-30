{-# LANGUAGE TypeApplications, DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Persist.TH.SharedPrimaryKeySpec where

import TemplateTestImports

import Data.Time
import Data.Proxy
import Test.Hspec
import Database.Persist
import Database.Persist.EntityDef
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH

share [ mkPersist sqlSettings ] [persistLowerCase|

User
    name    String

Profile
    Id      UserId
    email   String

Profile2
    Id      (Key User)
    email   String

DayKeyTable
    Id Day
    name Text

RefDayKey
    dayKey DayKeyTableId

|]

spec :: Spec
spec = describe "Shared Primary Keys" $ do
    let
        getSqlType :: PersistEntity a => Proxy a -> SqlType
        getSqlType p =
            case getEntityId (entityDef p) of
                EntityIdField fd ->
                    fieldSqlType fd
                _ ->
                    SqlOther "Composite Key"

        keyProxy :: Proxy a -> Proxy (Key a)
        keyProxy _ = Proxy

        sqlTypeEquivalent
            :: (PersistFieldSql (Key a), PersistEntity a)
            => Proxy a
            -> Expectation
        sqlTypeEquivalent proxy =
            sqlType (keyProxy proxy) `shouldBe` getSqlType proxy

        testSqlTypeEquivalent
            :: (PersistFieldSql (Key a), PersistEntity a)
            => Proxy a
            -> Spec
        testSqlTypeEquivalent prxy =
            it "has equivalent SqlType from sqlType and entityId" $
                sqlTypeEquivalent prxy
    describe "PersistFieldSql" $ do
        it "should match underlying key" $ do
            sqlType (Proxy @UserId)
                `shouldBe`
                    sqlType (Proxy @ProfileId)

    describe "User" $ do
        it "has default ID key, SqlInt64" $ do
            sqlType (Proxy @UserId)
                `shouldBe`
                    SqlInt64

        testSqlTypeEquivalent (Proxy @User)

    describe "Profile"  $ do
        it "has same ID key type as User" $ do
            sqlType (Proxy @ProfileId)
                `shouldBe`
                    sqlType (Proxy @UserId)
        testSqlTypeEquivalent(Proxy @Profile)

    describe "Profile2" $ do
        it "has same ID key type as User" $ do
            sqlType (Proxy @Profile2Id)
                `shouldBe`
                    sqlType (Proxy @UserId)
        testSqlTypeEquivalent (Proxy @Profile2)

    describe "getEntityId FieldDef" $ do
        it "should match underlying primary key" $ do
            getSqlType (Proxy @User)
                `shouldBe`
                    getSqlType (Proxy @Profile)

    describe "DayKeyTable" $ do
        testSqlTypeEquivalent (Proxy @DayKeyTable)

        it "sqlType has Day type" $ do
            sqlType (Proxy @Day)
                `shouldBe`
                    sqlType (Proxy @DayKeyTableId)

        it "getSqlType has Day type" $ do
            sqlType (Proxy @Day)
                `shouldBe`
                    getSqlType (Proxy @DayKeyTable)

    describe "RefDayKey" $ do
        let
            [dayKeyField] =
                getEntityFields (entityDef (Proxy @RefDayKey))
        testSqlTypeEquivalent (Proxy @RefDayKey)

        it "has same sqltype as underlying" $ do
            fieldSqlType dayKeyField
                `shouldBe`
                    sqlType (Proxy @Day)

        it "has the right fieldType" $ do
            fieldType dayKeyField
                `shouldBe`
                    FTTypeCon Nothing "DayKeyTableId"

        it "has the right type" $ do
            let
                _ =
                    refDayKeyDayKey
                        :: RefDayKey -> DayKeyTableId
                _ =
                    RefDayKeyDayKey
                        :: EntityField RefDayKey DayKeyTableId
            True `shouldBe` True

        it "has a foreign ref" $ do
            case fieldReference dayKeyField of
                ForeignRef refName -> do
                    refName `shouldBe` EntityNameHS "DayKeyTable"
                other ->
                    fail $ "expected foreign ref, got: " <> show other
