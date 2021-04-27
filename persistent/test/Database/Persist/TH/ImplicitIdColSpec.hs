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

module Database.Persist.TH.ImplicitIdColSpec where

import TemplateTestImports

import Data.Text (Text)

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)

do
    let
        uuidDef =
           mkImplicitIdDef @Text "uuid_generate_v1mc()"
        settings =
            setImplicitIdDef uuidDef sqlSettings

    mkPersist settings [persistLowerCase|

        User
            name    String
            age     Int

        |]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = describe "ImplicitIdColSpec" $ do
    describe "UserKey" $ do
        it "has type Text -> Key User" $ do
            let userKey = UserKey "Hello"
            pass

    describe "getEntityId" $ do
        let idField = getEntityId (entityDef (Nothing @User))
        it "has SqlString SqlType" $ asIO $ do
            fieldSqlType idField `shouldBe` SqlString
        it "has Text FieldType" $ asIO $ do
            fieldType idField `shouldBe` fieldTypeFromTypeable @Text
