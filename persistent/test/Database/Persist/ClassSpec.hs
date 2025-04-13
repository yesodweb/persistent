module Database.Persist.ClassSpec where

import Data.Time
import Database.Persist.Class
import Database.Persist.Types
import Test.Hspec

spec :: Spec
spec = describe "Class" $ do
    describe "PersistField" $ do
        describe "UTCTime" $ do
            it "fromPersistValue with ISO8601 format including UTC timezone Z (canonical)" $
                fromPersistValue (PersistText "2018-02-27T10:49:42.123Z")
                    `shouldBe` Right
                        (UTCTime (fromGregorian 2018 02 27) (timeOfDayToTime (TimeOfDay 10 49 42.123)))
            it "fromPersistValue with ISO8601 format no timezone (backwards-compatibility)" $
                fromPersistValue (PersistText "2018-02-27T10:49:42.123")
                    `shouldBe` Right
                        (UTCTime (fromGregorian 2018 02 27) (timeOfDayToTime (TimeOfDay 10 49 42.123)))
            it "fromPersistValue with pretty format (backwards-compatibility)" $
                fromPersistValue (PersistText "2018-02-27 10:49:42.123")
                    `shouldBe` Right
                        (UTCTime (fromGregorian 2018 02 27) (timeOfDayToTime (TimeOfDay 10 49 42.123)))
