{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module PersistTestPetCollarType where

import GHC.Generics
import Data.Aeson
import Database.Persist.TH
import Data.Text (Text)

data PetCollar = PetCollar {tag :: Text, bell :: Bool}
    deriving (Generic, Eq, Show)
instance ToJSON PetCollar
instance FromJSON PetCollar

derivePersistFieldJSON "PetCollar"
