module Database.Persist.ClassSpec where

import Database.Persist.Class
import Data.Time
import Database.Persist.Types
import Test.Hspec

spec :: Spec
spec = describe "Class" $ do
    describe "PersistField" $ do
        describe "UTCTime" $ do
            it "fromPersistValue with format" $
                fromPersistValue (PersistText "2018-02-27 10:49:42.123")
                    `shouldBe`
                        Right (UTCTime (fromGregorian 2018 02 27) (timeOfDayToTime (TimeOfDay 10 49 42.123)))

