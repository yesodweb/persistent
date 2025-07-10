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

import PgInit
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Fixed (Fixed(MkFixed))
import Database.Persist.Postgresql (PgInterval(..))
import Test.Hspec.QuickCheck
import Database.Persist.Postgresql.Interval ()
import qualified Database.PostgreSQL.Simple.Interval as Interval

share
    [mkPersist sqlSettings, mkMigrate "pgIntervalMigrate"]
    [persistLowerCase|
PgIntervalDb
    interval_field PgInterval
    deriving Eq
    deriving Show

IntervalDb
    interval_field Interval.Interval
    deriving Eq Show
|]

specs :: Spec
specs = do
    describe "Postgres Interval Property tests" $ do
        prop "Round trips" $ \int64 -> runConnAssert $ do
            let eg = PgIntervalDb . PgInterval . secondsToNominalDiffTime . MkFixed $ toInteger (int64 :: Int64) * 1000000
            rid <- insert eg
            r <- getJust rid
            liftIO $ r `shouldBe` eg

        prop "interval round trips" $ \(m, d, u) -> runConnAssert $ do
            let expected = IntervalDb $ Interval.MkInterval m d u
            key <- insert expected
            actual <- getJust key
            liftIO $ actual `shouldBe` expected
