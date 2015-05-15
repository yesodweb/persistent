{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
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
|]
#ifdef WITH_NOSQL
cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Foo ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Foo])
  deleteWhere ([] :: [Filter Bar])

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = describe "primary key reference" $ do
#ifdef WITH_NOSQL
  return ()
#else
#  ifdef WITH_MYSQL
  return ()
#  else
  it "insert a primary reference" $ db $ do
    kf <- insert $ Foo "name"
    kb <- insert $ Bar kf
    return ()
#  endif
#endif
