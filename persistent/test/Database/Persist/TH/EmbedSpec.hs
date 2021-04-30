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

|]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = describe "EmbedSpec" $ do
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

