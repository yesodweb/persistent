{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module MigrationOnlyTest (specs) where

import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import qualified Data.Text as T

import Init

#ifdef WITH_MONGODB
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migrateAll1"] [persistLowerCase|
#endif
TwoField1 sql=two_field
    field1 Int
    field2 T.Text
    field3 Bool Maybe
    deriving Eq Show
|]

#ifdef WITH_MONGODB
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migrateAll2", mkDeleteCascade sqlSettings] [persistLowerCase|
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
specs = describe "migration only" $ do
    it "works" $ asIO $ runResourceT $ runConn $ do
#ifndef WITH_MONGODB
        _ <- runMigrationSilent migrateAll1
        _ <- runMigrationSilent migrateAll2
#endif
        let tf = TwoField 5 "hello"
        tid <- insert tf
        mtf <- get tid
        liftIO $ mtf @?= Just tf
        deleteWhere ([] :: [Filter TwoField])

asIO :: IO a -> IO a
asIO = id
