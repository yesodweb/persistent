{-# LANGUAGE UndecidableInstances #-}
module SchemaTest where

import Init
import qualified PersistentTestModels as PTM

share [mkPersist persistSettings, mkMigrate "migration"] [persistLowerCase|
Person schema=my_special_schema
    name Text
    age Int
    weight Int
    deriving Show Eq
|]

cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Person ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter PTM.Person])

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "schema support" $ do
  it "insert a Person under non-default schema" $ runDb $ do
    insert_ $ Person "name" 1 2
    return ()
  it "insert PTM.Person and Person and check they end up in different tables" $ runDb $ do
    cleanDB
    insert_ $ Person "name" 1 2
    insert_ $ PTM.Person "name2" 3 Nothing
    schemaPersonCount <- count ([] :: [Filter Person])
    ptmPersoncount <- count ([] :: [Filter PTM.Person])
    -- both tables should contain only one record despite similarly named Entities
    schemaPersonCount + ptmPersoncount @== 2
