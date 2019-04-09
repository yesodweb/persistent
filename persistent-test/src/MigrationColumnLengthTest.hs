{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MigrationColumnLengthTest where

import Init

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
VaryingLengths
    field1 Int
    field2 T.Text sqltype=varchar(5)
|]

specsWith :: MonadIO m => RunDb SqlBackend m -> Spec
specsWith runDb =
  it "is idempotent" $ runDb $ do
      again <- getMigration migration
      liftIO $ again @?= []
