{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MigrationTest where

#ifndef WITH_MONGODB
import Database.Persist.Sqlite
#endif
import Database.Persist.TH
import qualified Data.Text as T

import Init

#ifdef WITH_MONGODB
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migrationMigrate", mkDeleteCascade sqlSettings] [persistLowerCase|
#endif
Target
    field1 Int
    field2 T.Text
    UniqueTarget field1 field2
    deriving Eq Show

Source
    field3 Int
    field4 TargetId
|]

#ifndef WITH_MONGODB
specs :: Spec
specs = describe "Migration" $ do
    it "is idempotent" $ db $ do
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
    it "really is idempotent" $ db $ do
      runMigration migrationMigrate
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
#endif
