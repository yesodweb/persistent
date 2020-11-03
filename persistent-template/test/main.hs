{-# LANGUAGE TypeApplications, DeriveGeneric, RecordWildCards #-}
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
{-# language DataKinds #-}

-- DeriveAnyClass is not actually used by persistent-template
-- But a long standing bug was that if it was enabled, it was used to derive instead of GeneralizedNewtypeDeriving
-- This was fixed by using DerivingStrategies to specify newtype deriving should be used.
-- This pragma is left here as a "test" that deriving works when DeriveAnyClass is enabled.
-- See https://github.com/yesodweb/persistent/issues/578
{-# LANGUAGE DeriveAnyClass #-}
module Main
  (
  -- avoid unused ident warnings
    module Main
  ) where

import Data.Int
import Data.Proxy
import Control.Applicative (Const (..))
import Data.Aeson
import Data.ByteString.Lazy.Char8 ()
import Data.Functor.Identity (Identity (..))
import Data.Text (Text, pack)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen)
import GHC.Generics (Generic)
import qualified Data.List as List
import Data.Coerce

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH
import TemplateTestImports

import qualified SharedPrimaryKeyTest
import qualified SharedPrimaryKeyTestImported
import qualified OverloadedLabelTest

share [mkPersist sqlSettings { mpsGeneric = False, mpsDeriveInstances = [''Generic] }, mkDeleteCascade sqlSettings { mpsGeneric = False }] [persistUpperCase|

Person json
    name Text
    age Int Maybe
    foo Foo
    address Address
    deriving Show Eq

HasSimpleCascadeRef
    person PersonId OnDeleteCascade
    deriving Show Eq

Address json
    street Text
    city Text
    zip Int Maybe
    deriving Show Eq
NoJson
    foo Text
    deriving Show Eq
|]

mkPersist sqlSettings [persistLowerCase|
HasPrimaryDef
    userId Int
    name String
    Primary userId

HasMultipleColPrimaryDef
    foobar Int
    barbaz String
    Primary foobar barbaz

HasIdDef
    Id Int
    name String

HasDefaultId
    name String

HasCustomSqlId
    Id String sql=my_id
    name String

SharedPrimaryKey
    Id (Key HasDefaultId)
    name String

SharedPrimaryKeyWithCascade
    Id (Key HasDefaultId) OnDeleteCascade
    name String

SharedPrimaryKeyWithCascadeAndCustomName
    Id (Key HasDefaultId) OnDeleteCascade sql=my_id
    name String
|]

share [mkPersist sqlSettings { mpsGeneric = False, mpsGenerateLenses = True }] [persistLowerCase|
Lperson json
    name Text
    age Int Maybe
    address Laddress
    deriving Show Eq
Laddress json
    street Text
    city Text
    zip Int Maybe
    deriving Show Eq
CustomPrimaryKey
    anInt Int
    Primary anInt
|]

arbitraryT :: Gen Text
arbitraryT = pack <$> arbitrary

instance Arbitrary Person where
    arbitrary = Person <$> arbitraryT <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Address where
    arbitrary = Address <$> arbitraryT <*> arbitraryT <*> arbitrary

main :: IO ()
main = hspec $ do
    OverloadedLabelTest.spec
    SharedPrimaryKeyTest.spec
    SharedPrimaryKeyTestImported.spec
    describe "HasDefaultId" $ do
        let FieldDef{..} =
                entityId (entityDef (Proxy @HasDefaultId))
        it "should have usual db name" $ do
            fieldDB `shouldBe` DBName "id"
        it "should have usual haskell name" $ do
            fieldHaskell `shouldBe` HaskellName "Id"
        it "should have correct underlying sql type" $ do
            fieldSqlType `shouldBe` SqlInt64
        it "persistfieldsql should be right" $ do
            sqlType (Proxy @HasDefaultIdId) `shouldBe` SqlInt64
        it "should have correct haskell type" $ do
            fieldType `shouldBe` FTTypeCon Nothing "HasDefaultIdId"

    describe "HasCustomSqlId" $ do
        let FieldDef{..} =
                entityId (entityDef (Proxy @HasCustomSqlId))
        it "should have custom db name" $ do
            fieldDB `shouldBe` DBName "my_id"
        it "should have usual haskell name" $ do
            fieldHaskell `shouldBe` HaskellName "id"
        it "should have correct underlying sql type" $ do
            fieldSqlType `shouldBe` SqlString
        it "should have correct haskell type" $ do
            fieldType `shouldBe` FTTypeCon Nothing "String"
    describe "HasIdDef" $ do
        let FieldDef{..} =
                entityId (entityDef (Proxy @HasIdDef))
        it "should have usual db name" $ do
            fieldDB `shouldBe` DBName "id"
        it "should have usual haskell name" $ do
            fieldHaskell `shouldBe` HaskellName "id"
        it "should have correct underlying sql type" $ do
            fieldSqlType `shouldBe` SqlInt64
        it "should have correct haskell type" $ do
            fieldType `shouldBe` FTTypeCon Nothing "Int"

    describe "SharedPrimaryKey" $ do
        let sharedDef = entityDef (Proxy @SharedPrimaryKey)
            FieldDef{..} =
                entityId sharedDef
        it "should have usual db name" $ do
            fieldDB `shouldBe` DBName "id"
        it "should have usual haskell name" $ do
            fieldHaskell `shouldBe` HaskellName "id"
        it "should have correct underlying sql type" $ do
            fieldSqlType `shouldBe` SqlInt64
        it "should have correct haskell type" $ do
            fieldType `shouldBe` FTApp (FTTypeCon Nothing "Key") (FTTypeCon Nothing "HasDefaultId")
        it "should have correct sql type from PersistFieldSql" $ do
            sqlType (Proxy @SharedPrimaryKeyId)
                `shouldBe`
                    SqlInt64
        it "should have same sqlType as underlying record" $ do
            sqlType (Proxy @SharedPrimaryKeyId)
                `shouldBe`
                    sqlType (Proxy @HasDefaultIdId)
        it "should be a coercible newtype" $ do
            coerce @Int64 3
                `shouldBe`
                    SharedPrimaryKeyKey (toSqlKey 3)

        it "is a newtype" $ do
            pkNewtype sqlSettings sharedDef
                `shouldBe`
                    True

    describe "SharedPrimaryKeyWithCascade" $ do
        let FieldDef{..} =
                entityId (entityDef (Proxy @SharedPrimaryKeyWithCascade))
        it "should have usual db name" $ do
            fieldDB `shouldBe` DBName "id"
        it "should have usual haskell name" $ do
            fieldHaskell `shouldBe` HaskellName "id"
        it "should have correct underlying sql type" $ do
            fieldSqlType `shouldBe` SqlInt64
        it "should have correct haskell type" $ do
            fieldType
                `shouldBe`
                    FTApp (FTTypeCon Nothing "Key") (FTTypeCon Nothing "HasDefaultId")
        it "should have cascade in field def" $ do
            fieldCascade `shouldBe` noCascade { fcOnDelete = Just Cascade }

    describe "OnCascadeDelete" $ do
        let subject :: FieldDef
            Just subject =
                List.find ((HaskellName "person" ==) . fieldHaskell)
                $ entityFields
                $ simpleCascadeDef
            simpleCascadeDef =
                entityDef (Proxy :: Proxy HasSimpleCascadeRef)
            expected =
                FieldCascade
                    { fcOnDelete = Just Cascade
                    , fcOnUpdate = Nothing
                    }
        describe "entityDef" $ do
            it "works" $ do
                simpleCascadeDef
                    `shouldBe`
                        EntityDef
                            { entityHaskell = HaskellName "HasSimpleCascadeRef"
                            , entityDB = DBName "HasSimpleCascadeRef"
                            , entityId =
                                FieldDef
                                    { fieldHaskell = HaskellName "Id"
                                    , fieldDB = DBName "id"
                                    , fieldType = FTTypeCon Nothing "HasSimpleCascadeRefId"
                                    , fieldSqlType = SqlInt64
                                    , fieldReference =
                                        ForeignRef (HaskellName "HasSimpleCascadeRef") (FTTypeCon (Just "Data.Int") "Int64")
                                    , fieldAttrs = []
                                    , fieldStrict = True
                                    , fieldComments = Nothing
                                    , fieldCascade = noCascade
                                    }
                            , entityAttrs = []
                            , entityFields =
                                [ FieldDef
                                    { fieldHaskell = HaskellName "person"
                                    , fieldDB = DBName "person"
                                    , fieldType = FTTypeCon Nothing "PersonId"
                                    , fieldSqlType = SqlInt64
                                    , fieldAttrs = []
                                    , fieldStrict = True
                                    , fieldReference =
                                        ForeignRef
                                            (HaskellName "Person")
                                            (FTTypeCon (Just "Data.Int") "Int64")
                                    , fieldCascade =
                                        FieldCascade { fcOnUpdate = Nothing, fcOnDelete = Just Cascade }
                                    , fieldComments = Nothing
                                    }
                                ]
                            , entityUniques = []
                            , entityForeigns = []
                            , entityDerives =  ["Show", "Eq"]
                            , entityExtra = mempty
                            , entitySum = False
                            , entityComments = Nothing
                            }
        it "has the cascade on the field def" $ do
            fieldCascade subject `shouldBe` expected
        it "doesn't have any extras" $ do
            entityExtra simpleCascadeDef
                `shouldBe`
                    mempty

    describe "hasNaturalKey" $ do
        let subject :: PersistEntity a => Proxy a -> Bool
            subject p = hasNaturalKey (entityDef p)
        it "is True for Primary keyword" $ do
            subject (Proxy @HasPrimaryDef)
                `shouldBe`
                    True
        it "is True for multiple Primary columns " $ do
            subject (Proxy @HasMultipleColPrimaryDef)
                `shouldBe`
                    True
        it "is False for Id keyword" $ do
            subject (Proxy @HasIdDef)
                `shouldBe`
                    False
        it "is False for unspecified/default id" $ do
            subject (Proxy @HasDefaultId)
                `shouldBe`
                    False
    describe "hasCompositePrimaryKey" $ do
        let subject :: PersistEntity a => Proxy a -> Bool
            subject p = hasCompositePrimaryKey (entityDef p)
        it "is False for Primary with single column" $ do
            subject (Proxy @HasPrimaryDef)
                `shouldBe`
                    False
        it "is True for multiple Primary columns " $ do
            subject (Proxy @HasMultipleColPrimaryDef)
                `shouldBe`
                    True
        it "is False for Id keyword" $ do
            subject (Proxy @HasIdDef)
                `shouldBe`
                    False
        it "is False for unspecified/default id" $ do
            subject (Proxy @HasDefaultId)
                `shouldBe`
                    False

    describe "JSON serialization" $ do
        prop "to/from is idempotent" $ \person ->
            decode (encode person) == Just (person :: Person)
        it "decode" $
            decode "{\"name\":\"Michael\",\"age\":27,\"foo\":\"Bar\",\"address\":{\"street\":\"Narkis\",\"city\":\"Maalot\"}}" `shouldBe` Just
                (Person "Michael" (Just 27) Bar $ Address "Narkis" "Maalot" Nothing)
    describe "JSON serialization for Entity" $ do
        let key = PersonKey 0
        prop "to/from is idempotent" $ \person ->
            decode (encode (Entity key person)) == Just (Entity key (person :: Person))
        it "decode" $
            decode "{\"id\": 0, \"name\":\"Michael\",\"age\":27,\"foo\":\"Bar\",\"address\":{\"street\":\"Narkis\",\"city\":\"Maalot\"}}" `shouldBe` Just
                (Entity key (Person "Michael" (Just 27) Bar $ Address "Narkis" "Maalot" Nothing))
    it "lens operations" $ do
        let street1 = "street1"
            city1 = "city1"
            city2 = "city2"
            zip1 = Just 12345
            address1 = Laddress street1 city1 zip1
            address2 = Laddress street1 city2 zip1
            name1 = "name1"
            age1 = Just 27
            person1 = Lperson name1 age1 address1
            person2 = Lperson name1 age1 address2
        (person1 ^. lpersonAddress) `shouldBe` address1
        (person1 ^. (lpersonAddress . laddressCity)) `shouldBe` city1
        (person1 & ((lpersonAddress . laddressCity) .~ city2)) `shouldBe` person2
    describe "Derived Show/Read instances" $ do
        -- This tests confirms https://github.com/yesodweb/persistent/issues/1104 remains fixed
        it "includes the name of the newtype when showing/reading a Key, i.e. uses the stock strategy when deriving Show/Read" $ do
            show (PersonKey 0) `shouldBe` "PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 0}}"
            read (show (PersonKey 0)) `shouldBe` PersonKey 0

            show (CustomPrimaryKeyKey 0) `shouldBe` "CustomPrimaryKeyKey {unCustomPrimaryKeyKey = 0}"
            read (show (CustomPrimaryKeyKey 0)) `shouldBe` CustomPrimaryKeyKey 0

(&) :: a -> (a -> b) -> b
x & f = f x

(^.) :: s
     -> ((a -> Const a b) -> (s -> Const a t))
     -> a
x ^. lens = getConst $ lens Const x

(.~) :: ((a -> Identity b) -> (s -> Identity t))
     -> b
     -> s
     -> t
lens .~ val = runIdentity . lens (\_ -> Identity val)
