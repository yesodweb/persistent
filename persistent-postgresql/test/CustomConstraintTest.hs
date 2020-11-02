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

import PgInit
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

specs :: Spec
specs = do
  describe "custom constraint used in migration" $ do
    it "custom constraint is actually created" $ runConnAssert $ do
      void $ runMigrationSilent customConstraintMigrate
      void $ runMigrationSilent customConstraintMigrate -- run a second time to ensure the constraint isn't dropped
      let query = T.concat ["SELECT DISTINCT COUNT(*) "
                           ,"FROM information_schema.constraint_column_usage ccu, "
                           ,"information_schema.key_column_usage kcu, "
                           ,"information_schema.table_constraints tc "
                           ,"WHERE tc.constraint_type='FOREIGN KEY' "
                           ,"AND kcu.constraint_name=tc.constraint_name "
                           ,"AND ccu.constraint_name=kcu.constraint_name "
                           ,"AND kcu.ordinal_position=1 "
                           ,"AND ccu.table_name=? "
                           ,"AND ccu.column_name=? "
                           ,"AND kcu.table_name=? "
                           ,"AND kcu.column_name=? "
                           ,"AND tc.constraint_name=?"]
      [Single exists] <- rawSql query [PersistText "custom_constraint1"
                                      ,PersistText "id"
                                      ,PersistText "custom_constraint2"
                                      ,PersistText "cc_id"
                                      ,PersistText "custom_constraint"]
      liftIO $ 1 @?= (exists :: Int)

    it "allows multiple constraints on a single column" $ runConnAssert $ do
      void $ runMigrationSilent customConstraintMigrate
      -- | Here we add another foreign key on the same column where the default one already exists. In practice, this could be a compound key with another field.
      rawExecute "ALTER TABLE \"custom_constraint3\" ADD CONSTRAINT \"extra_constraint\" FOREIGN KEY(\"cc_id1\") REFERENCES \"custom_constraint1\"(\"id\")" []
      -- | This is where the error is thrown in `getColumn`
      void $ getMigration customConstraintMigrate
      pure ()
