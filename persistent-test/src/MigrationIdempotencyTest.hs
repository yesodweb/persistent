{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module MigrationIdempotencyTest where

import qualified Data.Text as T

import Database.Persist.TH
import Init

share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
Idempotency
    field1 Int
    field2 T.Text sqltype=varchar(5)
    field3 T.Text sqltype=mediumtext
    field4 T.Text sqltype=longtext
    field5 T.Text sqltype=mediumblob
    field6 T.Text sqltype=longblob
    field7 Double sqltype=double(6,5)
|]

specsWith :: (MonadIO m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "MySQL migration with backend-specific sqltypes" $ do
  it "is idempotent" $ runDb $ do
      again <- getMigration migration
      liftIO $ again @?= []
