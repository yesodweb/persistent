{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NullsNotDistinctTest where

import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Postgresql.Internal
import Database.Persist.TH
import qualified Test.Hspec as Hspec

import PgInit

-- The standard unique constraint (allowing multiple NULLs) is migrated on every
-- supported PostgreSQL version, so it lives in its own migration that is always
-- safe to run.
share
    [mkPersist sqlSettings, mkMigrate "standardUniqueMigrate"]
    [persistLowerCase|
  -- Standard unique constraint (allows multiple NULLs)
  StandardUnique
    name Text
    email Text Maybe
    UniqueStandardEmail name email !force
    deriving Eq Show
|]

-- These entities use NULLS NOT DISTINCT, which is only valid on PostgreSQL 15+.
-- They are kept in a separate migration so that it is only ever run when the
-- server supports the feature (see 'main.hs' and the version-gated tests below).
-- Running this migration against PostgreSQL < 15 raises a syntax error.
share
    [mkPersist sqlSettings, mkMigrate "nullsNotDistinctMigrate"]
    [persistLowerCase|
  -- Unique constraint with NULLS NOT DISTINCT (PostgreSQL 15+)
  -- This should prevent multiple NULLs
  NullsNotDistinctUnique
    name Text
    email Text Maybe
    UniqueNNDEmail name email !nullsNotDistinct
    deriving Eq Show

  -- Multiple nullable fields with NULLS NOT DISTINCT
  MultiFieldNND
    fieldA Text
    fieldB Text Maybe
    fieldC Int Maybe
    UniqueMultiNND fieldA fieldB fieldC !nullsNotDistinct
    deriving Eq Show
|]

-- Helper to check PostgreSQL version
getPostgresVersion :: (MonadIO m) => ReaderT SqlBackend m (Maybe Int)
getPostgresVersion = do
    result <- rawSql "SELECT current_setting('server_version_num')::integer" []
    case result of
        [Single version] -> return $ Just version
        _ -> return Nothing

isPostgres15OrHigher :: (MonadIO m) => ReaderT SqlBackend m Bool
isPostgres15OrHigher = do
    mVersion <- getPostgresVersion
    case mVersion of
        Just version -> return $ version >= 150000 -- PostgreSQL 15.0
        Nothing -> return False

cleanStandard :: (MonadIO m) => ReaderT SqlBackend m ()
cleanStandard = deleteWhere ([] :: [Filter StandardUnique])

-- Only safe to call on PostgreSQL 15+, where the NND tables have been migrated.
cleanNND :: (MonadIO m) => ReaderT SqlBackend m ()
cleanNND = do
    deleteWhere ([] :: [Filter NullsNotDistinctUnique])
    deleteWhere ([] :: [Filter MultiFieldNND])

specs :: Spec
specs = describe "NULLS NOT DISTINCT support" $ do
    let
        runDb = runConnAssert

    it "generates correct SQL for NULLS NOT DISTINCT constraint" $ do
        let
            alterWithNND =
                AddUniqueConstraint
                    (ConstraintNameDB "unique_nnd_email")
                    [FieldNameDB "name", FieldNameDB "email"]
                    ["!nullsNotDistinct"]

        let
            alterWithoutNND =
                AddUniqueConstraint
                    (ConstraintNameDB "unique_standard_email")
                    [FieldNameDB "name", FieldNameDB "email"]
                    ["!force"]

        let
            tableName = EntityNameDB "test_table"
        let
            sqlWithNND = showAlterTable tableName alterWithNND
        let
            sqlWithoutNND = showAlterTable tableName alterWithoutNND

        sqlWithNND
            `Hspec.shouldBe` "ALTER TABLE \"test_table\" ADD CONSTRAINT \"unique_nnd_email\" UNIQUE NULLS NOT DISTINCT(\"name\",\"email\")"

        sqlWithoutNND
            `Hspec.shouldBe` "ALTER TABLE \"test_table\" ADD CONSTRAINT \"unique_standard_email\" UNIQUE(\"name\",\"email\")"

    describe "runtime behavior" $ do
        it "standard unique allows multiple NULLs" $ do
            runDb $ do
                cleanStandard

                -- These should both succeed with standard unique
                _ <- insert $ StandardUnique "user1" Nothing
                _ <- insert $ StandardUnique "user2" Nothing

                -- Verify both were inserted
                count1 <- count [StandardUniqueName ==. "user1"]
                count2 <- count [StandardUniqueName ==. "user2"]

                liftIO $ do
                    count1 `Hspec.shouldBe` 1
                    count2 `Hspec.shouldBe` 1

        it "standard unique prevents duplicate non-NULLs" $
            -- Both inserts run in the same transaction so the constraint
            -- violation propagates out of runDb for shouldThrow to catch.
            ( runDb $ do
                    cleanStandard
                    _ <- insert $ StandardUnique "user1" (Just "test@example.com")
                    _ <- insert $ StandardUnique "user1" (Just "test@example.com")
                    return ()
                )
                `Hspec.shouldThrow` Hspec.anyException

        it
            "standard unique getBy returns Nothing for NULL values (backwards compatibility)"
            $ do
                runDb $ do
                    cleanStandard

                    -- Insert a record with NULL email
                    _ <- insert $ StandardUnique "user1" Nothing

                    -- getBy with NULL should return Nothing (standard SQL behavior)
                    -- This ensures backwards compatibility - without !nullsNotDistinct,
                    -- getBy cannot find NULL values
                    result <- getBy $ UniqueStandardEmail "user1" Nothing

                    liftIO $ result `Hspec.shouldBe` Nothing

                    -- Verify that getBy still works for non-NULL values
                    k2 <- insert $ StandardUnique "user2" (Just "test@example.com")
                    result2 <- getBy $ UniqueStandardEmail "user2" (Just "test@example.com")

                    liftIO $ case result2 of
                        Just (Entity key _) -> key `Hspec.shouldBe` k2
                        Nothing -> Hspec.expectationFailure "getBy should find non-NULL values"

        -- The NULLS NOT DISTINCT tables are only migrated on PostgreSQL 15+, so we
        -- detect support once here and only build the feature tests when the table
        -- actually exists. This means a failing/absent migration can never be
        -- mistaken for a passing (shouldThrow) assertion.
        supportsNND <- Hspec.runIO $ runResourceT $ runConn_ isPostgres15OrHigher
        describe "PostgreSQL 15+ features" $
            if not supportsNND
                then
                    it "are skipped (requires PostgreSQL 15 or higher)" $
                        Hspec.pendingWith "Requires PostgreSQL 15 or higher"
                else do
                    it "NULLS NOT DISTINCT prevents multiple NULLs" $
                        -- Same name and email twice; the second insert must violate
                        -- the unique constraint. Both run in one transaction.
                        ( runDb $ do
                                cleanNND
                                _ <- insert $ NullsNotDistinctUnique "user1" Nothing
                                _ <- insert $ NullsNotDistinctUnique "user1" Nothing
                                return ()
                            )
                            `Hspec.shouldThrow` Hspec.anyException

                    it "NULLS NOT DISTINCT with multiple nullable fields" $ do
                        -- Different NULL patterns are still distinct and should succeed
                        runDb $ do
                            cleanNND
                            _ <- insert $ MultiFieldNND "test1" Nothing Nothing
                            _ <- insert $ MultiFieldNND "test1" (Just "value") Nothing
                            _ <- insert $ MultiFieldNND "test1" Nothing (Just 42)

                            count' <- count ([] :: [Filter MultiFieldNND])
                            liftIO $ count' `Hspec.shouldBe` 3

                        -- The same NULL pattern twice should violate the constraint
                        ( runDb $ do
                                cleanNND
                                _ <- insert $ MultiFieldNND "test1" Nothing Nothing
                                _ <- insert $ MultiFieldNND "test1" Nothing Nothing
                                return ()
                            )
                            `Hspec.shouldThrow` Hspec.anyException

                    it "getBy finds NULL values with NULLS NOT DISTINCT" $
                        runDb $ do
                            cleanNND

                            -- Insert with NULL
                            k1 <- insert $ NullsNotDistinctUnique "user1" Nothing

                            -- With our runtime detection, getBy now uses
                            -- IS NOT DISTINCT FROM for entities with
                            -- !nullsNotDistinct, allowing it to find NULL values
                            result <- getBy $ UniqueNNDEmail "user1" Nothing

                            liftIO $ case result of
                                Just (Entity key _) -> key `Hspec.shouldBe` k1
                                Nothing ->
                                    Hspec.expectationFailure
                                        "getBy should find NULL values when !nullsNotDistinct is set"

                    it "migration generates a NULLS NOT DISTINCT constraint" $
                        runDb $ do
                            -- Read PostgreSQL's catalog for the generated constraint.
                            constraints :: [(Single Text, Single Text)] <-
                                rawSql
                                    "SELECT conname, pg_get_constraintdef(oid) \
                                    \FROM pg_constraint \
                                    \WHERE conrelid = 'nulls_not_distinct_unique'::regclass \
                                    \  AND contype = 'u'"
                                    []

                            let
                                hasNND =
                                    any
                                        ( \(Single _, Single def) ->
                                            "NULLS NOT DISTINCT" `T.isInfixOf` def
                                        )
                                        constraints

                            liftIO $ hasNND `Hspec.shouldBe` True
