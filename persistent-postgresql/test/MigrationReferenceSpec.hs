{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MigrationReferenceSpec where

import PgInit

import Control.Monad.Trans.Writer (censor, mapWriterT)
import Data.Text (Text, isInfixOf)

share
    [mkPersist sqlSettings, mkMigrate "referenceMigrate"]
    [persistLowerCase|

LocationCapabilities
    Id Text
    bio Text

LocationCapabilitiesPrintingProcess
    locationCapabilitiesId LocationCapabilitiesId

LocationCapabilitiesPrintingFinish
    locationCapabilitiesId LocationCapabilitiesId

LocationCapabilitiesSubstrate
    locationCapabilitiesId LocationCapabilitiesId

|]

spec :: Spec
spec = describe "MigrationReferenceSpec" $ do
    it "works" $ runConnAssert $ do
        let
            noForeignKeys :: CautiousMigration -> CautiousMigration
            noForeignKeys = filter ((not . isReference) . snd)

            onlyForeignKeys :: CautiousMigration -> CautiousMigration
            onlyForeignKeys = filter (isReference . snd)

            isReference :: Text -> Bool
            isReference migration = "REFERENCES" `isInfixOf` migration

        runMigration $
            mapWriterT (censor noForeignKeys) $
                referenceMigrate

        runMigration $
            mapWriterT (censor onlyForeignKeys) $
                referenceMigrate
