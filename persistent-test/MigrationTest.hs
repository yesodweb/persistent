{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module MigrationTest where

import Database.Persist.Sqlite
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
    deriving Eq Show

Source
    field3 Int
    field4 TargetId
|]
specs :: Spec
specs = describe "Migration" $ do
#ifndef WITH_MONGODB
    it "is idempotent" $ db $ do
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
#endif
