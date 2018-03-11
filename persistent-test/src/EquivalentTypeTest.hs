{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module EquivalentTypeTest (specs) where

import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.TH
#ifdef WITH_POSTGRESQL
import qualified Data.Text as T
#endif

import Init

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migrateAll1"] [persistLowerCase|
#endif
EquivalentType sql=equivalent_types
    field1 Int
#ifdef WITH_POSTGRESQL
    field2 T.Text sqltype=text
    field3 T.Text sqltype=us_postal_code
#endif
    deriving Eq Show
|]

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migrateAll2"] [persistLowerCase|
#endif
EquivalentType2 sql=equivalent_types
    field1 Int
#ifdef WITH_POSTGRESQL
    field2 T.Text
    field3 T.Text sqltype=us_postal_code
#endif
    deriving Eq Show
|]

specs :: Spec
specs = describe "doesn't migrate equivalent types" $ do
    it "works" $ asIO $ runResourceT $ runConn $ do

#ifdef WITH_POSTGRESQL
        _ <- rawExecute "DROP DOMAIN IF EXISTS us_postal_code CASCADE" []
        _ <- rawExecute "CREATE DOMAIN us_postal_code AS TEXT CHECK(VALUE ~ '^\\d{5}$')" []
#endif

#ifndef WITH_NOSQL
        _ <- runMigrationSilent migrateAll1
        xs <- getMigration migrateAll2
        liftIO $ xs @?= []
#else
        return ()
#endif


asIO :: IO a -> IO a
asIO = id
