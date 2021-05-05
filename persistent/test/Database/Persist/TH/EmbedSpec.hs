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

module Database.Persist.TH.EmbedSpec where

import TemplateTestImports

import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)
import Database.Persist.Types
import Database.Persist.Types
import Database.Persist.EntityDef
import Database.Persist.EntityDef.Internal (toEmbedEntityDef)

mkPersist sqlSettings [persistLowerCase|

Thing
    name String
    foo  String MigrationOnly

    deriving Eq Show

EmbedThing
    someThing Thing

    deriving Eq Show

SelfEmbed
    name Text
    self SelfEmbed Maybe
    deriving Eq Show

MutualEmbed
    thing MutualTarget

MutualTarget
    thing [MutualEmbed]

ModelWithList
    names [Text]

HasMap
    map (M.Map T.Text T.Text)
    deriving Show Eq Read Ord

MapIdValue
    map (M.Map T.Text (Key Thing))
    deriving Show Eq Read Ord

|]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = describe "EmbedSpec" $ do
    describe "ModelWithList" $ do
        let
            edef =
                entityDef $ Proxy @ModelWithList
            [fieldDef] =
                getEntityFields edef
        it "has the right type" $ do
            fieldType fieldDef
                `shouldBe`
                    FTList (FTTypeCon Nothing "Text")
        it "has the right sqltype" $ do
            fieldSqlType fieldDef
                `shouldBe`
                    SqlString
    describe "MapIdValue" $ do
        let
            edef =
                entityDef $ Proxy @MapIdValue
            [fieldDef] =
                getEntityFields edef
        it "has the right type" $ do
            fieldType fieldDef
                `shouldBe`
                    ( FTTypeCon (Just "M") "Map"
                        `FTApp`
                        FTTypeCon (Just "T") "Text"
                        `FTApp`
                        (FTTypeCon Nothing "Key"
                            `FTApp`
                            FTTypeCon Nothing "Thing"
                        )
                    )
        it "has the right sqltype" $ do
            fieldSqlType fieldDef
                `shouldBe`
                    SqlString
    describe "HasMap" $ do
        let
            edef =
                entityDef $ Proxy @HasMap
            [fieldDef] =
                getEntityFields edef
        it "has the right type" $ do
            fieldType fieldDef
                `shouldBe`
                    ( FTTypeCon (Just "M") "Map"
                    `FTApp`
                    FTTypeCon (Just "T") "Text"
                    `FTApp`
                    FTTypeCon (Just "T") "Text"
                    )
        it "has the right sqltype" $ do
            fieldSqlType fieldDef
                `shouldBe`
                    SqlString

    describe "SomeThing" $ do
        let
            edef =
                entityDef $ Proxy @Thing
        describe "toEmbedEntityDef" $ do
            let
                embedDef =
                    toEmbedEntityDef edef
            it "should have the same field count as Haskell fields" $ do
                length (embeddedFields embedDef)
                    `shouldBe`
                        length (getEntityFields edef)

    describe "EmbedThing" $ do
        it "generates the right constructor" $ do
            let embedThing :: EmbedThing
                embedThing = EmbedThing (Thing "asdf")
            pass

    describe "SelfEmbed" $ do
        let
            edef =
                entityDef $ Proxy @SelfEmbed
        describe "fieldReference" $ do
            let
                [nameField, selfField] = getEntityFields edef
            it "has self reference" $ do
                fieldReference selfField
                    `shouldBe`
                        NoReference
        describe "toEmbedEntityDef" $ do
            let
                embedDef =
                    toEmbedEntityDef edef
            it "has the same field count as regular def" $ do
                length (getEntityFields edef)
                    `shouldBe`
                        length (embeddedFields embedDef)

