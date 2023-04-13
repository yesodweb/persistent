{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module MigrationColumnLengthTest where

import qualified Data.Text as T

import Init

share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
VaryingLengths
    field1 Int
    field2 T.Text sqltype=varchar(5)
    field3 T.Text collate=en_US.utf8
|]

specsWith :: MonadIO m => RunDb SqlBackend m -> Spec
specsWith runDb = do
  it "is idempotent" $ runDb $ do
      again <- getMigration migration
      liftIO $ again @?= []
