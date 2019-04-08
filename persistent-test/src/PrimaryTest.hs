{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module PrimaryTest where

import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
#if WITH_NOSQL
mkPersist persistSettings { mpsGeneric = False } [persistUpperCase|
#else
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "migration"] [persistLowerCase|
#endif
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
#ifdef WITH_NOSQL
cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Foo ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Foo])
  deleteWhere ([] :: [Filter Bar])

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

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

specs :: Spec
specs =
#ifdef WITH_NOSQL
  return ()
#else
#  ifdef WITH_MYSQL
  return ()
#  else
  specsWith db
#  endif
#endif
