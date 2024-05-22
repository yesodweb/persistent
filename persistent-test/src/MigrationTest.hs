{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module MigrationTest where

import Database.Persist.TH
import qualified Data.Text as T
import Data.Time

import Init

share [mkPersist sqlSettings, mkMigrate "migrationMigrate"] [persistLowerCase|
Target
    field1 Int
    field2 T.Text
    UniqueTarget field1 field2
    deriving Eq Show

Source
    field3 Int
    field4 TargetId

CustomSqlId
    pk      Int   sql=id
    Primary pk
|]

share [mkPersist sqlSettings, mkMigrate "migrationAddCol"] [persistLowerCase|
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

share [mkPersist sqlSettings, mkMigrate "migrationWithDefaultTime"] [persistLowerCase|
TimeDefault
    field3 Int
    extra  UTCTime default=current_timestamp
    field4 Target1Id
|]

specsWith :: (MonadUnliftIO m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "Migration" $ do
    it "is idempotent" $ runDb $ do
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
    it "really is idempotent" $ runDb $ do
      void $ runMigrationSilent migrationMigrate
      void $ runMigrationSilent migrationMigrate
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
    it "can add an extra column" $ runDb $ do
      -- Failing test case for #735.  Foreign-key checking, switched on in
      -- version 2.6.1, caused persistent-sqlite to generate a `references`
      -- constraint in a *temporary* table during migration, which fails.
      void $ runMigrationSilent migrationAddCol
      again <- getMigration migrationAddCol
      liftIO $ again @?= []
    it "is idempotent (default time example)" $ runDb $ do
      void $ runMigrationSilent migrationWithDefaultTime
      again <- getMigration migrationWithDefaultTime
      liftIO $ again @?= ["ALTER TABLE \"time_default\" ALTER COLUMN \"extra\" SET DEFAULT current_timestamp"]
