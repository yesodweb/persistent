{-# LANGUAGE DataKinds #-}
--
-- DeriveAnyClass is not actually used by persistent-template
-- But a long standing bug was that if it was enabled, it was used to derive instead of GeneralizedNewtypeDeriving
-- This was fixed by using DerivingStrategies to specify newtype deriving should be used.
-- This pragma is left here as a "test" that deriving works when DeriveAnyClass is enabled.
-- See https://github.com/yesodweb/persistent/issues/578
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.ForeignRefSpec where

import Control.Applicative (Const (..))
import Data.Aeson
import Data.ByteString.Lazy.Char8 ()
import Data.Coerce
import Data.Functor.Identity (Identity (..))
import Data.Int
import qualified Data.List as List
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen)

import Database.Persist
import Database.Persist.EntityDef.Internal
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH
import TemplateTestImports

mkPersist
    sqlSettings
    [persistLowerCase|

HasCustomName sql=custom_name
    name Text

ForeignTarget
    name Text
    deriving Eq Show

ForeignSource
    name Text
    foreignTargetId ForeignTargetId
    Foreign ForeignTarget fk_s_t foreignTargetId

ForeignPrimary
    name Text
    Primary name
    deriving Eq Show

ForeignPrimarySource
    name Text
    Foreign ForeignPrimary fk_name_target name

NullableRef
    name Text Maybe
    Foreign ForeignPrimary fk_nullable_ref name

ParentImplicit
    name Text

ChildImplicit
    name Text
    parent ParentImplicitId OnDeleteCascade OnUpdateCascade

ChildImplicitUnspecified
    name Text
    parent ParentImplicitId

ChildImplicitNoAction
    name Text
    parent ParentImplicitId OnDeleteNoAction OnUpdateNoAction

ParentExplicit
    name Text
    Primary name

ChildExplicit
    name Text
    Foreign ParentExplicit OnDeleteCascade OnUpdateCascade fkparent name

ChildExplicitNoAction
    name Text
    Foreign ParentExplicit OnDeleteNoAction OnUpdateNoAction fkparent name

ChildExplicitUnspecified
    name Text
    Foreign ParentExplicit fkparent name
|]

spec :: Spec
spec = describe "ForeignRefSpec" $ do
    describe "HasCustomName" $ do
        let
            edef =
                entityDef $ Proxy @HasCustomName
        it "should have a custom db name" $ do
            entityDB edef
                `shouldBe` EntityNameDB "custom_name"

    it "should compile" $ do
        True `shouldBe` True

    describe "ForeignPrimarySource" $ do
        let
            fpsDef =
                entityDef $ Proxy @ForeignPrimarySource
            [foreignDef] =
                entityForeigns fpsDef
        it "has the right type" $ do
            foreignPrimarySourceFk_name_target (ForeignPrimarySource "asdf")
                `shouldBe` ForeignPrimaryKey "asdf"

    describe "Unspecified" $ do
        describe "Explicit" $ do
            let
                parentDef =
                    entityDef $ Proxy @ParentExplicit
                childDef =
                    entityDef $ Proxy @ChildExplicitUnspecified
                childForeigns =
                    entityForeigns childDef
            it "should have a single foreign reference defined" $ do
                case entityForeigns childDef of
                    [ForeignDef{..}] ->
                        foreignFieldCascade
                            `shouldBe` FieldCascade
                                { fcOnUpdate = Nothing
                                , fcOnDelete = Nothing
                                }
                    as ->
                        expectationFailure . mconcat $
                            [ "(Explicit) Expected one foreign reference on childDef, "
                            , "got: "
                            , show as
                            ]

        describe "Implicit" $ do
            let
                parentDef =
                    entityDef $ Proxy @ParentImplicit
                childDef =
                    entityDef $ Proxy @ChildImplicitUnspecified
                childFields =
                    entityFields childDef
            describe "ChildImplicitUnspecified" $ do
                case childFields of
                    [nameField, parentIdField] -> do
                        it "parentId has reference" $ do
                            fieldReference parentIdField
                                `shouldBe` ForeignRef (EntityNameHS "ParentImplicit")
                            fieldCascade parentIdField
                                `shouldBe` FieldCascade
                                    { fcOnUpdate = Nothing
                                    , fcOnDelete = Nothing
                                    }
                    as ->
                        error . mconcat $
                            [ "(Implicit) Expected one foreign reference on childDef, "
                            , "got: "
                            , show as
                            ]

    describe "NoAction" $ do
        describe "Explicit" $ do
            let
                parentDef =
                    entityDef $ Proxy @ParentExplicit
                childDef =
                    entityDef $ Proxy @ChildExplicitNoAction
                childForeigns =
                    entityForeigns childDef
            it "should have a single foreign reference defined" $ do
                case entityForeigns childDef of
                    [ForeignDef{..}] ->
                        foreignFieldCascade
                            `shouldBe` FieldCascade
                                { fcOnUpdate = Just NoAction
                                , fcOnDelete = Just NoAction
                                }
                    as ->
                        expectationFailure . mconcat $
                            [ "(Explicit) Expected one foreign reference on childDef, "
                            , "got: "
                            , show as
                            ]

        describe "Implicit" $ do
            let
                parentDef =
                    entityDef $ Proxy @ParentImplicit
                childDef =
                    entityDef $ Proxy @ChildImplicitNoAction
                childFields =
                    entityFields childDef
            describe "ChildImplicitNoAction" $ do
                case childFields of
                    [nameField, parentIdField] -> do
                        it "parentId has reference" $ do
                            fieldReference parentIdField
                                `shouldBe` ForeignRef (EntityNameHS "ParentImplicit")
                            fieldCascade parentIdField
                                `shouldBe` FieldCascade
                                    { fcOnUpdate = Just NoAction
                                    , fcOnDelete = Just NoAction
                                    }
                    as ->
                        error . mconcat $
                            [ "(Implicit) Expected one foreign reference on childDef, "
                            , "got: "
                            , show as
                            ]

    describe "Cascade" $ do
        describe "Explicit" $ do
            let
                parentDef =
                    entityDef $ Proxy @ParentExplicit
                childDef =
                    entityDef $ Proxy @ChildExplicit
                childForeigns =
                    entityForeigns childDef
            it "should have a single foreign reference defined" $ do
                case entityForeigns childDef of
                    [ForeignDef{..}] ->
                        foreignFieldCascade
                            `shouldBe` FieldCascade
                                { fcOnUpdate = Just Cascade
                                , fcOnDelete = Just Cascade
                                }
                    as ->
                        expectationFailure . mconcat $
                            [ "(Explicit) Expected one foreign reference on childDef, "
                            , "got: "
                            , show as
                            ]
            let
                [ForeignDef{..}] =
                    childForeigns

            describe "ChildExplicit" $ do
                it "should have the right target table" $ do
                    foreignRefTableHaskell
                        `shouldBe` EntityNameHS "ParentExplicit"
                    foreignRefTableDBName
                        `shouldBe` EntityNameDB "parent_explicit"
                it "should have the right cascade behavior" $ do
                    foreignFieldCascade
                        `shouldBe` FieldCascade
                            { fcOnUpdate =
                                Just Cascade
                            , fcOnDelete =
                                Just Cascade
                            }
                it "is not nullable" $ do
                    foreignNullable `shouldBe` False
                it "is to the Primary key" $ do
                    foreignToPrimary `shouldBe` True

        describe "Implicit" $ do
            let
                parentDef =
                    entityDef $ Proxy @ParentImplicit
                childDef =
                    entityDef $ Proxy @ChildImplicit
                childFields =
                    entityFields childDef
            describe "ChildImplicit" $ do
                case childFields of
                    [nameField, parentIdField] -> do
                        it "parentId has reference" $ do
                            fieldReference parentIdField
                                `shouldBe` ForeignRef (EntityNameHS "ParentImplicit")
                            fieldCascade parentIdField
                                `shouldBe` FieldCascade
                                    { fcOnUpdate = Just Cascade
                                    , fcOnDelete = Just Cascade
                                    }
                    as ->
                        error . mconcat $
                            [ "(Implicit) Expected one foreign reference on childDef, "
                            , "got: "
                            , show as
                            ]
