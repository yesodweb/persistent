{-# LANGUAGE TemplateHaskell #-}
module PersistTestPetType where

import Database.Persist.TH

data PetType = Cat | Dog
    deriving (Show, Read, Eq)
derivePersistField "PetType"
