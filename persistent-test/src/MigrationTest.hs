{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module MigrationTest where

import Database.Persist.TH
import qualified Data.Text as T

import Init

share [mkPersist sqlSettings, mkMigrate "migrationMigrate", mkDeleteCascade sqlSettings] [persistLowerCase|
Target
    field1 Int
    field2 T.Text
    UniqueTarget field1 field2
    deriving Eq Show

Source
    field3 Int
    field4 TargetId

PreUnique sql=pre_unique
    field5 Int
    field6 T.Text

|]

share [mkPersist sqlSettings, mkMigrate "migrationAddCol", mkDeleteCascade sqlSettings] [persistLowerCase|
Target1 sql=target
    field1 Int
    field2 T.Text
    UniqueTarget1 field1 field2
    deriving Eq Show

Source1 sql=source
    field3 Int
    extra  Int
    field4 Target1Id

|]

share [mkPersist sqlSettings, mkMigrate "addPrimKey", mkDeleteCascade sqlSettings] [persistLowerCase|
FromRawMigration
    name T.Text
    age  Int

    Primary name
|]

share [mkPersist sqlSettings, mkMigrate "addUniqKey", mkDeleteCascade sqlSettings] [persistLowerCase|

PostUnique sql=pre_unique
    field5 Int
    field6 T.Text

    UniquField5 field5

|]

dropTables :: MonadIO m => SqlPersistT m ()
dropTables = do
    rawExecute "DROP TABLE IF EXISTS source" []
    rawExecute "DROP TABLE IF EXISTS target" []
    rawExecute "DROP TABLE IF EXISTS pre_unique" []
    rawExecute "DROP TABLE IF EXISTS from_raw_migration" []

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "Migration" $ before_ (runDb dropTables) $ do
    it "is idempotent" $ runDb $ do
        _ <- runMigration migrationMigrate
        again <- getMigration migrationMigrate
        liftIO $ again @?= []
    it "can add an extra column" $ runDb $ do
        -- Failing test case for #735.  Foreign-key checking, switched on in
        -- version 2.6.1, caused persistent-sqlite to generate a `references`
        -- constraint in a *temporary* table during migration, which fails.
        _ <- runMigration migrationMigrate
        _ <- runMigration migrationAddCol
        again <- getMigration migrationAddCol
        liftIO $ again @?= []
    describe "Add Unique Key constraint" $ do
        it "should not be considered safe" $ runDb $ do
            _ <- runMigration migrationMigrate
            Right migration <- parseMigration addUniqKey
            liftIO $ migration
                `shouldSatisfy`
                    (\cm -> True `elem` map fst cm)

    xdescribe "Add Primary key constraint on raw table" $ do
        it "should not be considered safe" $ runDb $ do
            rawExecute "CREATE TABLE from_raw_migration (name VARCHAR NOT NULL, age INT8 NOT NULL)" []
            Right migration <- parseMigration addPrimKey
            liftIO $ migration
                `shouldSatisfy`
                    (\cm -> True `elem` map fst cm)

        it "works" $ runDb $ do
            rawExecute "CREATE TABLE from_raw_migration (name VARCHAR NOT NULL, age INT NOT NULL)" []
            Right migration <- parseMigration addPrimKey
            () <- runMigration addPrimKey
            again <- getMigration addPrimKey
            liftIO $ again @?= []
