{-# LANGUAGE TypeApplications, DeriveGeneric #-}
{-# LANGUAGE DataKinds, ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module SharedPrimaryKeyTestImported where

import TemplateTestImports

import Data.Proxy
import Test.Hspec
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH

import SharedPrimaryKeyTest (User, UserId)

share [ mkPersist sqlSettings ] [persistLowerCase|

Profile
    Id      UserId
    email   String

|]

-- This test is very similar to the one in SharedPrimaryKeyTest, but it is
-- able to use 'UserId' directly, since the type is imported from another
-- module.
spec :: Spec
spec = describe "Shared Primary Keys Imported" $ do

    describe "PersistFieldSql" $ do
        it "should match underlying key" $ do
            sqlType (Proxy @UserId)
                `shouldBe`
                    sqlType (Proxy @ProfileId)

    describe "entityId FieldDef" $ do
        it "should match underlying primary key" $ do
            let getSqlType :: PersistEntity a => Proxy a -> SqlType
                getSqlType =
                    fieldSqlType . entityId . entityDef
            getSqlType (Proxy @User)
                `shouldBe`
                    getSqlType (Proxy @Profile)
