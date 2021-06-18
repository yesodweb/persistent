{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MigrationReferenceSpec where

import PgInit

import Control.Monad.Trans.Writer (censor, mapWriterT)
import Data.Text (Text, isInfixOf)

share [mkPersist sqlSettings, mkMigrate "referenceMigrate"] [persistLowerCase|

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

        runMigration
            $ mapWriterT (censor noForeignKeys)
            $ referenceMigrate

        runMigration
            $ mapWriterT (censor onlyForeignKeys)
            $ referenceMigrate
