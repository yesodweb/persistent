{-# LANGUAGE DataKinds #-}
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

module Database.Persist.TH.JsonEncodingSpec where

import TemplateTestImports

import Data.Aeson
import Data.Text (Text)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Database.Persist.EntityDef
import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)
import Database.Persist.Types

mkPersist
    sqlSettings
    [persistLowerCase|
JsonEncoding json
    name Text
    age  Int
    Primary name
    deriving Show Eq

JsonEncoding2 json
    name Text
    age Int
    blood Text
    Primary name blood
    deriving Show Eq

JsonEncMigrationOnly json
    name Text
    age  Int
    foo  Text MigrationOnly
|]

instance Arbitrary JsonEncoding where
    arbitrary = JsonEncoding <$> arbitrary <*> arbitrary

instance Arbitrary JsonEncoding2 where
    arbitrary = JsonEncoding2 <$> arbitrary <*> arbitrary <*> arbitrary

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = describe "JsonEncodingSpec" $ do
    let
        subject =
            JsonEncoding "Bob" 32
        subjectEntity =
            Entity (JsonEncodingKey (jsonEncodingName subject)) subject

    it "encodes without an ID field" $ do
        toJSON subjectEntity
            `shouldBe` object
                [ ("name", String "Bob")
                , ("age", toJSON (32 :: Int))
                , ("id", String "Bob")
                ]

    it "decodes without an ID field" $ do
        let
            json_ =
                encode . object $
                    [ ("name", String "Bob")
                    , ("age", toJSON (32 :: Int))
                    ]
        eitherDecode json_
            `shouldBe` Right subjectEntity

    it "has informative decoder errors" $ do
        let
            json_ = encode Null
        (eitherDecode json_ :: Either String JsonEncoding)
            `shouldBe` Left
                "Error in $: parsing JsonEncoding failed, expected Object, but encountered Null"

    prop "works with a Primary" $ \jsonEncoding -> do
        let
            ent =
                Entity (JsonEncodingKey (jsonEncodingName jsonEncoding)) jsonEncoding
        decode (encode ent)
            `shouldBe` Just ent

    prop "excuse me what" $ \j@JsonEncoding{..} -> do
        let
            ent =
                Entity (JsonEncodingKey jsonEncodingName) j
        toJSON ent
            `shouldBe` object
                [ ("name", toJSON jsonEncodingName)
                , ("age", toJSON jsonEncodingAge)
                , ("id", toJSON jsonEncodingName)
                ]

    prop "round trip works with composite key" $ \j@JsonEncoding2{..} -> do
        let
            key = JsonEncoding2Key jsonEncoding2Name jsonEncoding2Blood
            ent =
                Entity key j
        decode (encode ent)
            `shouldBe` Just ent

    prop "works with a composite key" $ \j@JsonEncoding2{..} -> do
        let
            key = JsonEncoding2Key jsonEncoding2Name jsonEncoding2Blood
            ent =
                Entity key j
        toJSON ent
            `shouldBe` object
                [ ("name", toJSON jsonEncoding2Name)
                , ("age", toJSON jsonEncoding2Age)
                , ("blood", toJSON jsonEncoding2Blood)
                , ("id", toJSON key)
                ]
