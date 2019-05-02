{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main
  (
  -- avoid unused ident warnings
    module Main
  ) where

import Control.Applicative (Const (..))
import Data.Aeson
import Data.ByteString.Lazy.Char8 ()
import Data.Functor.Identity (Identity (..))
import Data.Text (Text, pack)
import qualified Data.List as List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen)

import Database.Persist
import Database.Persist.TH
import TemplateTestImports
import Entities


share [mkPersist sqlSettings { mpsGeneric = False }, mkDeleteCascade sqlSettings { mpsGeneric = False }] entityDefs

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
|]

arbitraryT :: Gen Text
arbitraryT = pack <$> arbitrary

instance Arbitrary Person where
    arbitrary = Person <$> arbitraryT <*> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary Address where
    arbitrary = Address <$> arbitraryT <*> arbitraryT <*> arbitrary

main :: IO ()
main = hspec $ do
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
    let Just thingEntity =
            List.find
                (\edef -> unHaskellName (entityHaskell edef ) == "Thing")
                entityDefs
    it "the loaded entity should be the same as the class method" $ do
        entityDef (Nothing :: Maybe Thing)
            `shouldBe`
                thingEntity
    it "should have the right id type" $ do
        entityId thingEntity
            `shouldBe`
                FieldDef
                    (HaskellName "id")
                    (DBName "id")
                    (FTTypeCon Nothing "Text")
                    SqlString
                    ["sql=id"]
                    True
                    (ForeignRef (HaskellName "Thing") (FTTypeCon Nothing "Text"))
                    Nothing

    let Just gearEntityDef =
            List.find
                (\edef -> unHaskellName (entityHaskell edef ) == "Gear")
                entityDefs
    it "the loaded entity should be the same as the class method" $ do
        entityDef (Nothing :: Maybe Gear)
            `shouldBe`
                gearEntityDef

    let [thingRefDef] = entityFields gearEntityDef

    it "should have the right foreign key type to Thing" $ do
        fieldSqlType thingRefDef
            `shouldBe`
                fieldSqlType (entityId thingEntity)
        fieldReference thingRefDef
            `shouldBe`
                fieldReference (entityId thingEntity)

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
