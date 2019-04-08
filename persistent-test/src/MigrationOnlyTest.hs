{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
module MigrationOnlyTest (specs, specsWith, migrateAll1, migrateAll2) where

import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T

import Init

#ifdef WITH_NOSQL
mkPersist persistSettings { mpsGeneric = True } [persistUpperCase|
#else
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll1"] [persistLowerCase|
#endif
TwoField1 sql=two_field
    field1 Int
    field2 T.Text
    field3 Bool Maybe
    deriving Eq Show
|]

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll2", mkDeleteCascade sqlSettings] [persistLowerCase|
#endif
TwoField
    field1 Int
    field2 T.Text
    field3 Bool Maybe MigrationOnly
    deriving Eq Show

Referencing
    field1 Int
    field2 TwoFieldId MigrationOnly
|]

specs :: Spec
specs = specsWith runConn
#ifndef WITH_NOSQL
        (Just $ do
            runMigrationSilent migrateAll1
            runMigrationSilent migrateAll2)
#else
        Nothing
#endif

specsWith
    :: (MonadIO m, PersistQueryWrite backend, PersistStoreWrite backend, PersistQueryWrite (BaseBackend backend))
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -> Spec
specsWith runDb mmigrate = describe "MigrationOnly field" $ do
    it "doesn't have the field in the Haskell entity" $ asIO $ runDb $ do
        sequence_ mmigrate
        sequence_ mmigrate
        let tf = TwoField 5 "hello"
        tid <- insert tf
        mtf <- get tid
        liftIO $ mtf @?= Just tf
        deleteWhere ([] :: [Filter (TwoFieldGeneric backend)])
