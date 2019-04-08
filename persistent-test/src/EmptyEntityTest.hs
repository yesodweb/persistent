{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module EmptyEntityTest (specs, specsWith, migration) where

import Database.Persist.Sql
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT, ResourceT)

import Init

#ifdef WITH_NOSQL
mkPersist persistSettings { mpsGeneric = True } [persistUpperCase|
#else
-- Test lower case names
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migration"] [persistLowerCase|
#endif
EmptyEntity
|]

#ifdef WITH_NOSQL
cleanDB :: MonadIO m => ReaderT Context m ()
cleanDB = deleteWhere ([] :: [Filter EmptyEntity])
#endif

specs :: Spec
specs = describe "empty entity" $
    it "inserts" $ (id :: IO () -> IO ()) $ runResourceT $ runConn $ do
#ifndef WITH_NOSQL
        _ <- runMigrationSilent migration
        -- Ensure reading the data from the database works...
        _ <- runMigrationSilent migration
#endif
        x <- insert EmptyEntity
        Just EmptyEntity <- get x
        return ()

specsWith
    ::
    ( PersistStoreRead backend, PersistStoreWrite backend
    , PersistStoreWrite (BaseBackend backend)
    , MonadFail m, MonadIO m
    )
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -> Spec
specsWith runConn mmigrate = describe "empty entity" $
    it "inserts" $ asIO $ runConn $ do
        _ <- sequence_ mmigrate
        -- Ensure reading the data from the database works...
        _ <- sequence_ mmigrate
        x <- insert EmptyEntity
        Just EmptyEntity <- get x
        return ()
