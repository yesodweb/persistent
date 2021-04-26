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

module Database.Persist.TH.MigrationOnlySpec where

import TemplateTestImports

import Data.Text (Text)

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)

mkPersist sqlSettings [persistLowerCase|

HasMigrationOnly
    name String
    blargh Int MigrationOnly

|]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = describe "MigrationOnlySpec" $ do
    describe "HasMigrationOnly" $ do
        let
            edef =
                entityDef $ Proxy @HasMigrationOnly
        describe "getEntityFields" $ do
            it "has one field" $ do
                length (getEntityFields edef)
                    `shouldBe` 1
        describe "getEntityFieldsDatabase" $ do
            it "has two fields" $ do
                length (getEntityFieldsDatabase edef)
                    `shouldBe` 2
