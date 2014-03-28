{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module EmptyEntityTest (specs) where

import Database.Persist.Sql
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)

import Init

#ifdef WITH_MONGODB
mkPersist persistSettings [persistUpperCase|
#else
-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "dataTypeMigrate"] [persistLowerCase|
#endif
EmptyEntity
|]

cleanDB :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend EmptyEntity) => m ()
cleanDB = deleteWhere ([] :: [Filter EmptyEntity])

specs :: Spec
specs = describe "empty entity" $
    it "inserts" $ (id :: IO () -> IO ()) $ runResourceT $ runConn $ do
#ifndef WITH_MONGODB
        _ <- runMigrationSilent dataTypeMigrate
        -- Ensure reading the data from the database works...
        _ <- runMigrationSilent dataTypeMigrate
#endif
        x <- insert EmptyEntity
        Just EmptyEntity <- get x
        return ()
