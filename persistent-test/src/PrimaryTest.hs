{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module PrimaryTest where

import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "migration"] [persistLowerCase|
  Foo
    name String
    Primary name
  Bar
    quux FooId

  Trees sql=trees
    name String
    parent String Maybe
    Primary name
    Foreign Trees fkparent parent
|]


cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Foo ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Foo])
  deleteWhere ([] :: [Filter Bar])

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "primary key reference" $ do
  it "insert a primary reference" $ runDb $ do
    kf  <- insert $ Foo "name"
    _kb <- insert $ Bar kf
    return ()
  it "uses RawSql for a Primary key" $ runDb $ do
    key <- insert $ Foo "name"
    keyFromRaw <- rawSql "SELECT name FROM foo LIMIT 1" []
    [key] @== keyFromRaw
