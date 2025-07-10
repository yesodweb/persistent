{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances                      #-}
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
{-# LANGUAGE TypeOperators              #-}

module PgIntervalTest where

import PgInit
import Data.Time.Clock (NominalDiffTime)
import Database.Persist.Postgresql (PgInterval(..))
import Test.Hspec.QuickCheck
import qualified Database.Persist.Postgresql.Interval as Interval
import qualified Database.PostgreSQL.Simple.Interval as Interval

share [mkPersist sqlSettings, mkMigrate "pgIntervalMigrate"] [persistLowerCase|
PgIntervalDb
    interval_field PgInterval
    deriving Eq
    deriving Show

IntervalDb
    interval_field Interval.Interval
    deriving Eq Show
|]

-- Postgres Interval has a 1 microsecond resolution, while NominalDiffTime has
-- picosecond resolution. Round to the nearest microsecond so that we can be
-- fine in the tests.
truncate' :: NominalDiffTime -> NominalDiffTime
truncate' x = (fromIntegral (round (x * 10^6))) / 10^6

specs :: Spec
specs = do
    describe "Postgres Interval Property tests" $ do
        prop "Round trips" $ \time -> runConnAssert $ do
            let eg = PgIntervalDb $ PgInterval (truncate' time)
            rid <- insert eg
            r <- getJust rid
            liftIO $ r `shouldBe` eg

        prop "interval round trips" $ \(m, d, u) -> runConnAssert $ do
            let expected = IntervalDb $ Interval.MkInterval m d u
            key <- insert expected
            actual <- getJust key
            liftIO $ actual `shouldBe` expected
