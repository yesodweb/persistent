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

import MyInit
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
