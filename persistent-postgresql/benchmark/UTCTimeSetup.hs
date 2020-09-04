{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}

module UTCTimeSetup where

import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Data.Aeson (Value(..))
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Database.Persist.Postgresql.JSON()
import Test.Hspec
import Test.QuickCheck.Instances ()
import Data.Time
import Data.Text
import Chronos (OffsetDatetime)
import ChronosPersistent

share [mkPersist sqlSettings, mkMigrate "utcTimeBenchmarkMigration"] [persistLowerCase|
  UserWithTimestamps
    firstName Text
    lastName Text
    createdAt OffsetDatetime
    updatedAt OffsetDatetime
|]