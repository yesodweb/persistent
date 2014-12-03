{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module EmptyEntityTest (specs) where

import Database.Persist.Sql
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)

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
