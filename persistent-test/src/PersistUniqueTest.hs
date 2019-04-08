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

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "custom primary key" $ do
  it "getBy" $ runDb $ do
    let b = 5
    k <- insert $ Fo 3 b
    Just vk <- get k
    Just vu <- getBy (UniqueBar b)
    vu @== Entity k vk
  it "insertUniqueEntity" $ runDb $ do
    let fo = Fo 3 5
    Just (Entity _ insertedFoValue) <- insertUniqueEntity fo
    Nothing <- insertUniqueEntity fo
    fo @== insertedFoValue

specs :: Spec
specs =
#ifdef WITH_NOSQL
  return ()
#else
  specsWith db
#endif
