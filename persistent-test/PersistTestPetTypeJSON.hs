{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module PersistTestPetTypeJSON where

import GHC.Generics
import Data.Aeson
import Database.Persist.TH

data PetTypeJSON = Cat {name :: String}
                 | Dog {name :: String}
  deriving (Generic, Eq)
instance ToJSON PetTypeJSON
instance FromJSON PetTypeJSON

derivePersistFieldJSON "PetTypeJSON"
