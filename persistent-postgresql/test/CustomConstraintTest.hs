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
import Data.List (find)
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "customConstraintMigrate"] [persistLowerCase|
CustomConstraint1
    some_field Text
    deriving Show

CustomConstraint2
    cc_id CustomConstraint1Id constraint=custom_constraint
    deriving Show
|]

alterQuery :: Text
alterQuery = "ALTER TABLE \"custom_constraint2\" ADD CONSTRAINT \"custom_constraint\" FOREIGN KEY(\"cc_id\") REFERENCES \"custom_constraint1\"(\"id\")"

specs :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specs runDb = do
  describe "custom constraint used in migration" $ do
    it "alter table uses custom constraint name" $ runDb $ do
      ms <- getMigration customConstraintMigrate
      let alter = find (== alterQuery) ms
      liftIO $ alter @?= Just alterQuery
    it "custom constraint is actually created" $ runDb $ do
      runMigration customConstraintMigrate
      runMigration customConstraintMigrate -- run a second time to ensure the constraint isn't dropped
      let query = T.concat ["SELECT COUNT(*) "
                           ,"FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS "
                           ,"WHERE TABLE_NAME='custom_constraint2' "
                           ,"AND CONSTRAINT_NAME='custom_constraint'"]
      [Single exists] <- rawSql query []
      liftIO $ 1 @?= (exists :: Int)

