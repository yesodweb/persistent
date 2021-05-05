{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module GeneratedColumnTestSQL (specsWith) where

import Database.Persist.TH
import Init

share [mkPersist sqlSettings, mkMigrate "migrate1"] [persistLowerCase|
GenTest sql=gen_test
  fieldOne Text Maybe
  fieldTwo Text Maybe
  fieldThree Text Maybe generated=COALESCE(field_one,field_two)
  deriving Show Eq

MigrateTestV1 sql=gen_migrate_test
  sickness Int
  cromulence Int generated=5
|]

share [mkPersist sqlSettings, mkMigrate "migrate2"] [persistLowerCase|
MigrateTestV2 sql=gen_migrate_test
  sickness Int generated=3
  cromulence Int
|]

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDB = describe "PersistLiteral field" $ do
    it "should read a generated column" $ runDB $ do
        rawExecute "DROP TABLE IF EXISTS gen_test;" []
        rawExecute "DROP TABLE IF EXISTS gen_migrate_test;" []
        runMigration migrate1

        insert_ GenTest
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

    it "should support adding or removing generation expressions from columns" $ runDB $ do
        runMigration migrate2

        k2 <- insert $ MigrateTestV2 0 0
        Just (MigrateTestV2 sickness2 cromulence2) <- get k2
        liftIO $ sickness2 @?= 3
        liftIO $ cromulence2 @?= 0
