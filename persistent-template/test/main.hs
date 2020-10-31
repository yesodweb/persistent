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

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util
import Database.Persist.TH
import TemplateTestImports

import qualified SharedPrimaryKeyTest
import qualified SharedPrimaryKeyTestImported

share [mkPersist sqlSettings { mpsGeneric = False, mpsDeriveInstances = [''Generic] }, mkDeleteCascade sqlSettings { mpsGeneric = False }] [persistUpperCase|
Person json
    name Text
    age Int Maybe
    foo Foo
    address Address
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
    SharedPrimaryKeyTest.spec
    SharedPrimaryKeyTestImported.spec
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
