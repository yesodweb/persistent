{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Database.Persist.Sqlite.CompositeSpec where

import SqliteInit

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Conduit.List as CL
import Conduit
import Database.Persist.Sqlite
import System.IO (hClose)
import Control.Exception (handle, IOException, throwIO)
import System.IO.Temp (withSystemTempFile)
import qualified Data.Text as T
import qualified Lens.Micro as Lens

share [mkPersist sqlSettings, mkMigrate "compositeSetup"] [persistLowerCase|
SimpleComposite
    int Int
    text Text
    Primary text int
    deriving Show Eq

SimpleCompositeReference
    int Int
    text Text
    label Text
    Foreign SimpleComposite fk_simple_composite text int
    deriving Show Eq
|]

share [mkPersist sqlSettings, mkMigrate "compositeMigrateTest"] [persistLowerCase|
SimpleComposite2 sql=simple_composite
    int Int
    text Text
    new Int default=0
    Primary text int
    deriving Show Eq

SimpleCompositeReference2 sql=simple_composite_reference
    int Int
    text Text
    label Text
    Foreign SimpleComposite2 fk_simple_composite text int
    deriving Show Eq
|]

spec :: Spec
spec = describe "CompositeSpec" $ do
    it "properly migrates to a composite primary key (issue #669)" $ asIO $ runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
        void $ runMigrationSilent compositeSetup
        void $ runMigrationSilent compositeMigrateTest
        pure ()
    it "test migrating sparse composite primary keys (issue #1184)" $ asIO $ withSystemTempFile "test564.sqlite3"$ \fp h -> do
        hClose h
        let connInfo = Lens.set fkEnabled False $ mkSqliteConnectionInfo (T.pack fp)

        runSqliteInfo connInfo $ do
            void $ runMigrationSilent compositeSetup
            forM_ [(1,"foo"),(3,"bar")] $ \(intKey, strKey) -> do
                let key = SimpleCompositeKey strKey intKey
                insertKey key (SimpleComposite intKey strKey)
                insert (SimpleCompositeReference intKey strKey "test")

            validateForeignKeys

        runSqliteInfo connInfo $ do
            void $ runMigrationSilent compositeMigrateTest
            validateForeignKeys


validateForeignKeys
    :: (MonadResource m, MonadReader env m, BackendCompatible SqlBackend env)
    => m ()
validateForeignKeys = do
    violations <- map (T.pack . show) <$> runConduit (checkForeignKeys .| CL.consume)
    unless (null violations) . liftIO . throwIO $
        PersistForeignConstraintUnmet (T.unlines violations)
