{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
module CustomConstraintTest where

import MyInit
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "customConstraintMigrate"] [persistLowerCase|
CustomConstraint1
    some_field Text
    deriving Show

CustomConstraint2
    cc_id CustomConstraint1Id constraint=custom_constraint
    deriving Show

CustomConstraint3
    -- | This will lead to a constraint with the name custom_constraint3_cc_id1_fkey
    cc_id1 CustomConstraint1Id
    cc_id2 CustomConstraint1Id
    deriving Show
|]

clean :: MonadUnliftIO m => SqlPersistT m ()
clean = do
    rawExecute "drop table custom_constraint3" []
    rawExecute "drop table custom_constraint2" []
    rawExecute "drop table custom_constraint1" []

specs :: (MonadUnliftIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specs runDb = do
  describe "custom constraint used in migration" $ before_ (runDb $ void $ runMigrationSilent customConstraintMigrate) $ after_ (runDb clean) $ do

    it "custom constraint is actually created" $ runDb $ do
      runMigrationSilent customConstraintMigrate -- run a second time to ensure the constraint isn't dropped
      let query = T.concat ["SELECT COUNT(*) "
                           ,"FROM information_schema.key_column_usage "
                           ,"WHERE ordinal_position=1 "
                           ,"AND referenced_table_name=? "
                           ,"AND referenced_column_name=? "
                           ,"AND table_name=? "
                           ,"AND column_name=? "
                           ,"AND constraint_name=?"]
      [Single exists] <- rawSql query [PersistText "custom_constraint1"
                                      ,PersistText "id"
                                      ,PersistText "custom_constraint2"
                                      ,PersistText "cc_id"
                                      ,PersistText "custom_constraint"]
      liftIO $ 1 @?= (exists :: Int)

    it "allows multiple constraints on a single column" $ runDb $ do
      -- Here we add another foreign key on the same column where the
      -- default one already exists. In practice, this could be
      -- a compound key with another field.
      rawExecute "ALTER TABLE custom_constraint3 ADD CONSTRAINT extra_constraint FOREIGN KEY(cc_id1) REFERENCES custom_constraint1(id)" []
      -- This is where the error is thrown in `getColumn`
      _ <- getMigration customConstraintMigrate
      pure ()
