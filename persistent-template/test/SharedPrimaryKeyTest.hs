{-# LANGUAGE TypeApplications, DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
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

module SharedPrimaryKeyTest where

import TemplateTestImports

import Data.Proxy
import Test.Hspec
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH

share [ mkPersist sqlSettings ] [persistLowerCase|

User
    name    String

-- TODO: uncomment this out.
-- Profile
--     Id      UserId
--     email   String

Profile
    Id      (Key User)
    email   String

|]

spec :: Spec
spec = describe "Shared Primary Keys" $ do

    describe "PersistFieldSql" $ do
        it "should match underlying key" $ do
            sqlType (Proxy @UserId)
                `shouldBe`
                    sqlType (Proxy @ProfileId)

    describe "entityId FieldDef" $ do
        it "should match underlying primary key" $ do
            let userIdDef =
                    fieldSqlType $ entityId $ entityDef $ Proxy @User
                profileIdDef =
                    fieldSqlType $ entityId $ entityDef $ Proxy @Profile
            profileIdDef
                `shouldBe`
                    userIdDef
