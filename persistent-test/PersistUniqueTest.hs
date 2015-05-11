{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module PersistUniqueTest where

import Init
#ifndef WITH_NOSQL
import Control.Monad (void)
#endif

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
      deriving Show
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
    k <- insert $ Fo 3 5
    test <- get k -- works
    liftIO $ print test
    _ <- getBy (UniqueBar 5) -- exception
    return ()
#endif
