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

import Data.Fixed (Fixed (MkFixed), Micro, Pico)
import Data.Time.Clock (secondsToNominalDiffTime)
import Database.Persist.Postgresql (PgInterval (..))
import qualified Database.PostgreSQL.Simple.Interval as Interval
import PgInit
import Test.Hspec.QuickCheck

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

clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- Before version 15, PostgreSQL can't parse all possible intervals.
-- Each component is limited to the range of Int32.
-- So anything beyond 2,147,483,647 hours will fail to parse.

microsecondLimit :: Int64
microsecondLimit = 2147483647 * 60 * 60 * 1000000

specs :: Spec
specs = do
    describe "Postgres Interval Property tests" $ do
        prop "Round trips" $ \int64 -> runConnAssert $ do
            let
                eg =
                    PgIntervalDb
                        . PgInterval
                        . secondsToNominalDiffTime
                        . (realToFrac :: Micro -> Pico)
                        . MkFixed
                        . toInteger
                        $ clamp (-microsecondLimit) microsecondLimit int64
            rid <- insert eg
            r <- getJust rid
            liftIO $ r `shouldBe` eg

        prop "interval round trips" $ \(m, d, u) -> runConnAssert $ do
            let
                expected =
                    IntervalDb . Interval.MkInterval m d $
                        clamp (-microsecondLimit) microsecondLimit u
            key <- insert expected
            actual <- getJust key
            liftIO $ actual `shouldBe` expected
