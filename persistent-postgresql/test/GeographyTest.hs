{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GeographyTest (specs) where

import Control.Monad.Trans.Resource (runResourceT)
import GeographyType

import Database.Persist.TH
import PgInit

share [mkPersist sqlSettings, mkMigrate "migrateGeographyTest"] [persistLowerCase|
GeographyTest
    location Geo
|]

specs :: Spec
specs = describe "GeographyTest" $ do
    it "works" $ asIO $ runResourceT $ runConn $ do
        -- this test requires PostGIS to be installed
        extensions :: [Single PersistValue] <- rawSql "SELECT extname FROM pg_extension;" []
        when ((Single (PersistText "postgis")) `elem` extensions) $ do
          _ <- rawExecute "DROP TABLE IF EXISTS geography_test;" []

          _ <- runMigrationSilent migrateGeographyTest

          insert_ $ GeographyTest (toPoint 44 44)

          Just (Entity _ GeographyTest{..}) <- selectFirst [] []
          let (Geo pointByteString) = geographyTestLocation
          liftIO $ pointByteString @?= "'0101000020E610000000000000000046400000000000004640'"
