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

specsWith :: (MonadUnliftIO m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "Migration" $ do
    it "is idempotent" $ runDb $ do
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
    it "really is idempotent" $ runDb $ do
      runMigrationSilent migrationMigrate
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
    it "can add an extra column" $ runDb $ do
      -- Failing test case for #735.  Foreign-key checking, switched on in
      -- version 2.6.1, caused persistent-sqlite to generate a `references`
      -- constraint in a *temporary* table during migration, which fails.
      _ <- runMigrationSilent migrationAddCol
      again <- getMigration migrationAddCol
      liftIO $ again @?= []
