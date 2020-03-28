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
{-# LANGUAGE DeriveAnyClass             #-}

module PgIntervalTest where

import PgInit
import Control.Monad.IO.Unlift
import Data.Time.Clock (NominalDiffTime)
import Database.Persist.Postgresql (PgInterval(..))
import Test.Hspec.QuickCheck

share [mkPersist sqlSettings, mkMigrate "pgIntervalMigrate"] [persistLowerCase|
PgIntervalDb
    interval_field PgInterval
    deriving Eq
    deriving Show
|]

-- Postgres Interval has a 1 microsecond resolution, while NominalDiffTime has
-- picosecond resolution. Round to the nearest microsecond so that we can be
-- fine in the tests.
truncate' :: NominalDiffTime -> NominalDiffTime
truncate' x = (fromIntegral (round (x * 10^6))) / 10^6

specs :: (MonadUnliftIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specs runDb = describe "Postgres Interval Property tests" $
    prop "Round trips" $ \time -> runDb $ do
      let eg = PgIntervalDb $ PgInterval (truncate' time)
      rid <- insert eg
      r <- getJust rid
      liftIO $ r `shouldBe` eg
