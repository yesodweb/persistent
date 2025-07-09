{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module PgIntervalTest where

import Data.Time.Clock (NominalDiffTime)
import Database.Persist.Postgresql (PgInterval (..))
import PgInit
import Test.Hspec.QuickCheck

share
    [mkPersist sqlSettings, mkMigrate "pgIntervalMigrate"]
    [persistLowerCase|
PgIntervalDb
    interval_field PgInterval
    deriving Eq
    deriving Show
|]

-- Postgres Interval has a 1 microsecond resolution, while NominalDiffTime has
-- picosecond resolution. Round to the nearest microsecond so that we can be
-- fine in the tests.
truncate' :: NominalDiffTime -> NominalDiffTime
truncate' x = (fromIntegral (round (x * 10 ^ 6))) / 10 ^ 6

specs :: Spec
specs = do
    describe "Postgres Interval Property tests" $ do
        prop "Round trips" $ \time -> runConnAssert $ do
            let
                eg = PgIntervalDb $ PgInterval (truncate' time)
            rid <- insert eg
            r <- getJust rid
            liftIO $ r `shouldBe` eg
