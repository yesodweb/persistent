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
{-# LANGUAGE UndecidableInstances #-}
module LongIdentifierTest where

import Database.Persist.TH
import Init

-- This test creates very long identifier names. The generated foreign key is over the length limit for Postgres and MySQL
-- persistent-postgresql handles this by truncating foreign key names using the same algorithm that Postgres itself does (see 'refName' in Postgresql.hs)
-- MySQL currently doesn't run this test, and needs truncation logic for it to pass.
share [mkPersist sqlSettings, mkMigrate "migration", mkDeleteCascade sqlSettings] [persistLowerCase|
TableAnExtremelyFantasticallySuperLongNameParent
    field1 Int
TableAnExtremelyFantasticallySuperLongNameChild
    columnAnExtremelyFantasticallySuperLongNameParentId TableAnExtremelyFantasticallySuperLongNameParentId
|]

specsWith :: (MonadIO m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "Long identifiers" $ do
    -- See 'refName' in Postgresql.hs for why these tests are necessary.
    it "migrating is idempotent" $ runDb $ do
      again <- getMigration migration
      liftIO $ again @?= []
    it "migrating really is idempotent" $ runDb $ do
      runMigration migration
      again <- getMigration migration
      liftIO $ again @?= []
