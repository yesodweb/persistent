{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module EquivalentTypeTest (specsWith) where

import UnliftIO

import Database.Persist.TH
import Init

share [mkPersist sqlSettings, mkMigrate "migrateAll1"] [persistLowerCase|
EquivalentType sql=equivalent_types
    field1 Int
    deriving Eq Show
|]

share [mkPersist sqlSettings, mkMigrate "migrateAll2"] [persistLowerCase|
EquivalentType2 sql=equivalent_types
    field1 Int
    deriving Eq Show
|]

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDb = describe "doesn't migrate equivalent types" $ do
    it "works" $ runDb $ do
        _ <- runMigrationSilent migrateAll1
        xs <- getMigration migrateAll2
        liftIO $ xs @?= []
