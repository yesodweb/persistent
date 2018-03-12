{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module MigrationTest where

import Database.Persist.TH
import qualified Data.Text as T

import Init

#ifdef WITH_NOSQL
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

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migrationAddCol", mkDeleteCascade sqlSettings] [persistLowerCase|
#endif
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

#ifndef WITH_NOSQL
specs :: Spec
specs = describe "Migration" $ do
    it "is idempotent" $ db $ do
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
    it "really is idempotent" $ db $ do
      runMigration migrationMigrate
      again <- getMigration migrationMigrate
      liftIO $ again @?= []
    it "can add an extra column" $ db $ do
      -- Failing test case for #735.  Foreign-key checking, switched on in
      -- version 2.6.1, caused persistent-sqlite to generate a `references`
      -- constraint in a *temporary* table during migration, which fails.
      _ <- runMigration migrationAddCol
      again <- getMigration migrationAddCol
      liftIO $ again @?= []
#endif
