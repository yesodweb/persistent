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

module Database.Persist.TH.SharedPrimaryKeyImportedSpec where

import TemplateTestImports

import Data.Proxy
import Test.Hspec
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH
import Language.Haskell.TH
import Control.Monad.IO.Class

import Database.Persist.TH.SharedPrimaryKeySpec (User, UserId)

mkPersistWith sqlSettings $(discoverEntities) [persistLowerCase|

ProfileX
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
                    sqlType (Proxy @ProfileXId)

    describe "getEntityId FieldDef" $ do
        it "should match underlying primary key" $ do
            let
                getSqlType :: PersistEntity a => Proxy a -> SqlType
                getSqlType p =
                    case getEntityId (entityDef p) of
                        EntityIdField fd ->
                            fieldSqlType fd
                        _ ->
                            SqlOther "Composite Key"
            getSqlType (Proxy @User)
                `shouldBe`
                    getSqlType (Proxy @ProfileX)


    describe "foreign reference should work" $ do
        it "should have a foreign reference" $ do
            pendingWith "issue #1289"
            let
                Just fd =
                    getEntityIdField (entityDef (Proxy @ProfileX))
            fieldReference fd
                `shouldBe`
                    ForeignRef (EntityNameHS "User")
