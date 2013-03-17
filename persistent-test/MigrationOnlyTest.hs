{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module MigrationOnlyTest (specs) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
import Database.Persist.Sqlite
import Database.Persist.TH
#ifndef WITH_MONGODB
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Database.Persist.EntityDef
import Database.Persist.GenericSql.Raw
import qualified Data.Map as Map
#endif
import Control.Monad.Trans.Resource (runResourceT)
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import qualified Data.Text as T

import Init

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TwoField
    field1 Int
    field2 T.Text
    field3 Bool Maybe MigrationOnly
    deriving Eq Show
|]

specs :: Spec
specs = describe "migration only" $ do
    it "works" $ asIO $ runResourceT $ runConn $ do
#ifndef WITH_MONGODB
        _ <- runMigrationSilent migrateAll
#endif
        let tf = TwoField 5 "hello"
        tid <- insert tf
        mtf <- get tid
        liftIO $ mtf @?= Just tf

asIO :: IO a -> IO a
asIO = id
