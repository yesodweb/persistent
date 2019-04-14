{-# LANGUAGE ScopedTypeVariables #-}
module ReadWriteTest where

import Init
import PersistentTestModels


specsWith :: forall m. Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith originalRunDb = describe "ReadWriteTest" $ do
    let personFilters = [] :: [Filter Person]
    describe "SqlReadBackend" $ do
        let runDb :: RunDb SqlReadBackend m
            runDb = changeBackend SqlReadBackend originalRunDb
        it "type checks on all PersistStoreRead functions" $ do
            runDb $ do
                _ <- get (PersonKey 3)
                _ <- getMany [PersonKey 1, PersonKey 2]
                pure ()

        it "type checks on all PersistQueryRead functions" $ do
            runDb $ do
                _ <- selectList personFilters []
                _ <- count personFilters
                pure ()

        it "type checks on PersistUniqueRead functions" $ do
            runDb $ do
                _ <- getBy (PersonNameKey "Matt")
                pure ()

    describe "SqlWriteBackend" $ do
        let runDb :: RunDb SqlWriteBackend m
            runDb = changeBackend SqlWriteBackend originalRunDb

        it "type checks on PersistStoreWrite and Read functions" $ do
            runDb $ do
                let person = Person "Matt Parsons" 30 Nothing
                k <- insert person
                mperson <- get k
                Just person @== mperson

        it "type checks on PersistQueryWrite and Read functions" $ do
            runDb $ do
                _ <- selectList personFilters []
                updateWhere personFilters []

        it "type checks on PersistUniqueWrite/Read functions" $ do
            runDb $ do
                let name = "Matt Parsons New"
                    person = Person name 30 Nothing
                _mkey0 <- insertUnique person
                mkey1 <- insertUnique person
                mkey1 @== Nothing
                mperson <- selectFirst [PersonName ==. name] []
                fmap entityVal mperson @== Just person
