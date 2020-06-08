{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Instances ()
import Data.Text (Text)

import Database.Persist.Quasi
import Database.Persist.TH
import Data.UUID (UUID)
import Data.Time (UTCTime)
import Data.ByteString
import Data.Aeson (Value)


main :: IO ()
main = pure ()

share [mkPersist sqlSettings]
  $(persistFileWith lowerCaseSettings "models.persistentmodels")