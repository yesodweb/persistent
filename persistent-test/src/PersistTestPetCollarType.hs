{-# LANGUAGE DeriveGeneric #-}
module PersistTestPetCollarType where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Database.Persist.TH

data PetCollar = PetCollar {tag :: Text, bell :: Bool}
    deriving (Generic, Eq, Show)
instance ToJSON PetCollar
instance FromJSON PetCollar

derivePersistFieldJSON "PetCollar"
