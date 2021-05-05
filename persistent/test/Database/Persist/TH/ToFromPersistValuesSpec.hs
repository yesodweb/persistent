{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
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
--
-- DeriveAnyClass is not actually used by persistent-template
-- But a long standing bug was that if it was enabled, it was used to derive instead of GeneralizedNewtypeDeriving
-- This was fixed by using DerivingStrategies to specify newtype deriving should be used.
-- This pragma is left here as a "test" that deriving works when DeriveAnyClass is enabled.
-- See https://github.com/yesodweb/persistent/issues/578
{-# LANGUAGE DeriveAnyClass #-}

module Database.Persist.TH.ToFromPersistValuesSpec where

import TemplateTestImports

import Database.Persist.Sql.Util
import Database.Persist.Class.PersistEntity
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL

instance PersistFieldSql a => PersistFieldSql (NonEmpty a) where
    sqlType _ = SqlString

instance PersistField a => PersistField (NonEmpty a) where
    toPersistValue = toPersistValue . NEL.toList
    fromPersistValue pv = do
        xs <- fromPersistValue pv
        case xs of
            [] -> Left "PersistField: NonEmpty found unexpected Empty List"
            (l:ls) -> Right (l:|ls)

mkPersist sqlSettings [persistLowerCase|

NormalModel
    name Text
    age  Int
    deriving Eq Show

PrimaryModel
    name Text
    age Int
    Primary name age
    deriving Eq Show

IsMigrationOnly
    name Text
    age Int
    blargh Int MigrationOnly
    deriving Eq Show

HasListField
    names [Text]
    deriving Eq Show

HasNonEmptyListField
    names (NonEmpty Text)
    deriving Eq Show

HasNonEmptyListKeyField
    names (NonEmpty (Key NormalModel))
    deriving Eq Show
|]

spec :: Spec
spec = describe "{to,from}PersistValues" $ do
    let
        toPersistValues
            :: PersistEntity rec => rec -> [PersistValue]
        toPersistValues =
            map toPersistValue . toPersistFields

        subject
            :: (PersistEntity rec, Show rec, Eq rec)
            => rec
            -> [PersistValue]
            -> Spec
        subject model fields = do
            it "toPersistValues" $ do
                toPersistValues model
                    `shouldBe`
                        fields
            it "fromPersistValues" $ do
                fromPersistValues fields
                    `shouldBe`
                        Right model
    describe "NormalModel" $ do
        subject
            (NormalModel "hello" 30)
            [ PersistText "hello"
            , PersistInt64 30
            ]

    describe "PrimaryModel" $ do
        subject
            (PrimaryModel "hello" 30)
            [ PersistText "hello"
            , PersistInt64 30
            ]

    describe "IsMigrationOnly" $ do
        subject
            (IsMigrationOnly "hello" 30)
            [ PersistText "hello"
            , PersistInt64 30
            ]

    describe "mkInsertValues" $ do
        describe "NormalModel" $ do
            it "has all values" $ do
                mkInsertValues (NormalModel "hello" 30)
                    `shouldBe`
                        [ PersistText "hello"
                        , PersistInt64 30
                        ]
        describe "PrimaryModel" $ do
            it "has all values" $ do
                mkInsertValues (PrimaryModel "hello" 30)
                    `shouldBe`
                        [ PersistText "hello"
                        , PersistInt64 30
                        ]
        describe "IsMigrationOnly" $ do
            it "has all values" $ do
                mkInsertValues (IsMigrationOnly "hello" 30)
                    `shouldBe`
                        [ PersistText "hello"
                        , PersistInt64 30
                        ]
    describe "parseEntityValues" $ do
        let
            subject
                :: forall rec. (PersistEntity rec, Show rec, Eq rec)
                => [PersistValue]
                -> Entity rec
                -> Spec
            subject pvs rec =
                it "parses" $ do
                    parseEntityValues (entityDef (Proxy @rec)) pvs
                        `shouldBe`
                            Right rec
        describe "NormalModel" $ do
            subject
                [ PersistInt64 20
                , PersistText "hello"
                , PersistInt64 30
                ]
                Entity
                    { entityKey =
                        NormalModelKey 20
                    , entityVal =
                        NormalModel "hello" 30
                    }
        describe "PrimaryModel" $ do
            subject
                [ PersistText "hey"
                , PersistInt64 30
                ]
                Entity
                    { entityKey =
                        PrimaryModelKey "hey" 30
                    , entityVal =
                        PrimaryModel "hey" 30
                    }
        describe "IsMigrationOnly" $ do
            subject
                [ PersistInt64 20
                , PersistText "hello"
                , PersistInt64 30
                ]
                Entity
                    { entityKey =
                        IsMigrationOnlyKey 20
                    , entityVal =
                        IsMigrationOnly "hello" 30
                    }
    describe "entityValues" $ do
        let
            subject
                :: forall rec. (PersistEntity rec, Show rec, Eq rec)
                => [PersistValue]
                -> Entity rec
                -> Spec
            subject pvals entity = do
                it "renders as you would expect"$ do
                    entityValues entity
                        `shouldBe`
                            pvals
                it "round trips with parseEntityValues" $ do
                    parseEntityValues
                        (entityDef $ Proxy @rec)
                        (entityValues entity)
                        `shouldBe`
                            Right entity
        describe "NormalModel" $ do
            subject
                [ PersistInt64 10
                , PersistText "hello"
                , PersistInt64 20
                ]
                Entity
                    { entityKey =
                        NormalModelKey 10
                    , entityVal =
                        NormalModel "hello" 20
                    }
        describe "PrimaryModel" $ do
            subject
                [ PersistText "hello"
                , PersistInt64 20
                ]
                Entity
                    { entityKey =
                        PrimaryModelKey "hello" 20
                    , entityVal =
                        PrimaryModel "hello" 20
                    }
        describe "IsMigrationOnly" $ do
            subject
                [ PersistInt64 20
                , PersistText "hello"
                , PersistInt64 20
                ]
                Entity
                    { entityKey =
                        IsMigrationOnlyKey 20
                    , entityVal =
                        IsMigrationOnly "hello" 20
                    }

        describe "HasListField" $ do
            subject
                [ PersistInt64 10
                , PersistList [PersistText "hello"]
                ]
                Entity
                    { entityKey =
                        HasListFieldKey 10
                    , entityVal =
                        HasListField ["hello"]
                    }
        describe "HasNonEmptyListField" $ do
            subject
                [ PersistInt64 10
                , PersistList [PersistText "hello"]
                ]
                Entity
                    { entityKey =
                        HasNonEmptyListFieldKey 10
                    , entityVal =
                        HasNonEmptyListField (pure "hello")
                    }
        describe "HasNonEmptyListKeyField" $ do
            subject
                [ PersistInt64 5
                , PersistList [PersistInt64 4]
                ]
                Entity
                    { entityKey =
                        HasNonEmptyListKeyFieldKey 5
                    , entityVal =
                        HasNonEmptyListKeyField (pure (NormalModelKey 4))
                    }
