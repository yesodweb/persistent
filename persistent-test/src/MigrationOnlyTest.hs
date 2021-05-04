{-# LANGUAGE TypeApplications, UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MigrationOnlyTest (specsWith, migrateAll1, migrateAll2) where

import qualified Data.Text as T

import Database.Persist.TH
import Init
import Database.Persist.EntityDef

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll1"] [persistLowerCase|
TwoField1 sql=two_field
    field1 Int
    field2 T.Text
    field3 Bool Maybe
    deriving Eq Show
|]

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll2"] [persistLowerCase|
TwoField
    field1 Int
    field2 T.Text
    field3 Bool Maybe MigrationOnly
    deriving Eq Show

Referencing
    field1 Int
    field2 TwoFieldId MigrationOnly
|]

specsWith
    :: (MonadIO m, PersistQueryWrite backend, PersistStoreWrite backend, PersistQueryWrite (BaseBackend backend))
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -> Spec
specsWith runDb mmigrate = describe "MigrationOnly field" $ do
    let
        edef =
            entityDef $ Proxy @TwoField
    describe "getEntityFields" $ do
        let
            fields =
                getEntityFields edef
        it "should have two fields" $ do
            length fields `shouldBe` 2
        it "should not have any migration only fields" $ do
            fields `shouldSatisfy` all isHaskellField

    describe "getEntityFieldsDatabase" $ do
        let
            fields =
                getEntityFieldsDatabase edef
        it "should have three fields" $ do
            length fields `shouldBe` 3
        it "should have at one migration only field" $ do
            length (filter (not . isHaskellField) fields) `shouldBe` 1

    it "doesn't have the field in the Haskell entity" $ asIO $ runDb $ do
        sequence_ mmigrate
        sequence_ mmigrate
        let tf = TwoField 5 "hello"
        tid <- insert tf
        mtf <- get tid
        liftIO $ mtf @?= Just tf
        deleteWhere ([] :: [Filter (TwoFieldGeneric backend)])
