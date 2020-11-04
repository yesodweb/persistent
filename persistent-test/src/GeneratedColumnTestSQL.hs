{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module GeneratedColumnTestSQL (specsWith) where

import Database.Persist.TH
import Init
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrate1", mkDeleteCascade sqlSettings] [persistLowerCase|
GenTest sql=gen_test
  fieldOne Text Maybe
  fieldTwo Text Maybe
  fieldThree Text Maybe generated=COALESCE(field_one,field_two)
  deriving Show Eq

MigrateTestV1 sql=gen_migrate_test
  sickness Int
  cromulence Int generated=5
|]

share [mkPersist sqlSettings, mkMigrate "migrate2", mkDeleteCascade sqlSettings] [persistLowerCase|
MigrateTestV2 sql=gen_migrate_test
  sickness Int generated=3
  cromulence Int
|]

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDB = describe "PersistLiteral field" $ do
  it "should read a generated column" $ runDB $ do
    rawExecute "DROP TABLE IF EXISTS gen_test, gen_migrate_test;" []
    runMigration migrate1

    insert_ $ GenTest
      { genTestFieldOne = Just "like, literally this exact string"
      , genTestFieldTwo = Just "like, totally some other string"
      , genTestFieldThree = Nothing
      }
    Just (Entity _ GenTest{..}) <- selectFirst [] []
    liftIO $ genTestFieldThree @?= Just "like, literally this exact string"

    k1 <- insert $ MigrateTestV1 0 0
    Just (MigrateTestV1 sickness1 cromulence1) <- get k1
    liftIO $ sickness1 @?= 0
    liftIO $ cromulence1  @?= 5

    runMigration migrate2
    k2 <- insert $ MigrateTestV2 0 0
    Just (MigrateTestV2 sickness2 cromulence2) <- get k2
    liftIO $ sickness2 @?= 3
    liftIO $ cromulence2 @?= 0


-- specsWith :: (MonadUnliftIO m) => RunDb SqlBackend m -> Spec
-- specsWith runDb = describe "Migration" $ do
--     it "is idempotent" $ runDb $ do
--       again <- getMigration migrationMigrate
--       liftIO $ again @?= []
--     it "really is idempotent" $ runDb $ do
--       runMigrationSilent migrationMigrate
--       runMigrationSilent migrationMigrate
--       again <- getMigration migrationMigrate
--       liftIO $ again @?= []
--     it "can add an extra column" $ runDb $ do
--       -- Failing test case for #735.  Foreign-key checking, switched on in
--       -- version 2.6.1, caused persistent-sqlite to generate a `references`
--       -- constraint in a *temporary* table during migration, which fails.
--       _ <- runMigrationSilent migrationAddCol
--       again <- getMigration migrationAddCol
--       liftIO $ again @?= []
