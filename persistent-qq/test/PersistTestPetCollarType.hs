{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistTestPetCollarType where

import Data.Aeson
import Data.Text (Text)
import Database.Persist.TH
import GHC.Generics

data PetCollar = PetCollar {tag :: Text, bell :: Bool}
    deriving (Generic, Eq, Show)
instance ToJSON PetCollar
instance FromJSON PetCollar

derivePersistFieldJSON "PetCollar"
