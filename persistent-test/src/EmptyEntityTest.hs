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
module EmptyEntityTest (specs, specsWith) where

import Database.Persist.Sql
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT, ResourceT)

import Init

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "dataTypeMigrate"] [persistLowerCase|
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
        _ <- runMigrationSilent dataTypeMigrate
        -- Ensure reading the data from the database works...
        _ <- runMigrationSilent dataTypeMigrate
#endif
        x <- insert EmptyEntity
        Just EmptyEntity <- get x
        return ()

specsWith
    ::
    ( PersistEntityBackend entity ~ BaseBackend backend
    , PersistStoreRead backend, PersistStoreWrite backend
    , MonadFail m, MonadIO m
    , PersistEntity entity
    )
    => (ReaderT backend m () -> ResourceT IO ())
    -> Maybe (ReaderT backend m a)
    -> entity
    -> Spec
specsWith runConn mmigrate entity = describe "empty entity" $
    it "inserts" $ asIO $ runResourceT $ runConn $ do
        _ <- sequence_ mmigrate
        -- Ensure reading the data from the database works...
        _ <- sequence_ mmigrate
        x <- insert entity
        Just _entity' <- get x
        return ()
