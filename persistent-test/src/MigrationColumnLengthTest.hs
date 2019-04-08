{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MigrationColumnLengthTest where

import Database.Persist.TH
import qualified Data.Text as T

import Init

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
#endif
VaryingLengths
    field1 Int
    field2 T.Text sqltype=varchar(5)
|]

specsWith :: MonadIO m => RunDb SqlBackend m -> Spec
specsWith runDb =
  it "is idempotent" $ runDb $ do
      again <- getMigration migration
      liftIO $ again @?= []

specs :: Spec
specs = specsWith db
