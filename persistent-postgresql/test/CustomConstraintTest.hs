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
|]

specs :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specs runDb = do
  describe "custom constraint used in migration" $ do
    it "custom constraint is actually created" $ runDb $ do
      runMigration customConstraintMigrate
      runMigration customConstraintMigrate -- run a second time to ensure the constraint isn't dropped
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

