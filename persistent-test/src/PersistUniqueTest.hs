{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module PersistUniqueTest where

import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
#if WITH_NOSQL
mkPersist persistSettings { mpsGeneric = False } [persistUpperCase|
#else
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "migration"] [persistLowerCase|
#endif
  Fo
      foo Int
      bar Int
      Primary foo
      UniqueBar bar
      deriving Eq Show
|]
#ifdef WITH_NOSQL
cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Fo ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Fo])

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = describe "custom primary key" $ do
#ifdef WITH_NOSQL
  return ()
#else
  it "getBy" $ db $ do
    let b = 5
    k <- insert $ Fo 3 b
    Just vk <- get k
    Just vu <- getBy (UniqueBar b)
    vu @== Entity k vk
  it "insertUniqueEntity" $ db $ do
    let fo = Fo 3 5
    Just (Entity _ insertedFoValue) <- insertUniqueEntity fo
    Nothing <- insertUniqueEntity fo
    fo @== insertedFoValue
#endif
