{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module SumTypeTest (specsWith, sumTypeMigrate) where

import qualified Data.Text as T

import Database.Persist.TH
import Init

share [mkPersist persistSettings { mpsGeneric = True }, mkMigrate "sumTypeMigrate"] [persistLowerCase|
Bicycle
    brand T.Text
Car
    make T.Text
    model T.Text
+Vehicle
    bicycle BicycleId
    car CarId
|]

-- This is needed for mpsGeneric = True
-- The typical persistent user sets mpsGeneric = False
-- https://ghc.haskell.org/trac/ghc/ticket/8100
deriving instance Show (BackendKey backend) => Show (VehicleGeneric backend)
deriving instance Eq (BackendKey backend) => Eq (VehicleGeneric backend)

specsWith
    ::
    ( PersistQueryWrite backend
    , BaseBackend backend ~ backend
    , MonadIO m, MonadFail m
    )
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -- ^ Optional migrations for SQL backends
    -> Spec
specsWith runDb mmigrate = describe "sum types" $
    it "works" $ asIO $ runDb $ do
        sequence_ mmigrate
        car1 <- insert $ Car "Ford" "Thunderbird"
        car2 <- insert $ Car "Kia" "Rio"
        bike1 <- insert $ Bicycle "Shwinn"

        vc1 <- insert $ VehicleCarSum car1
        vc2 <- insert $ VehicleCarSum car2
        vb1 <- insert $ VehicleBicycleSum bike1

        x1 <- get vc1
        liftIO $ x1 @?= Just (VehicleCarSum car1)

        x2 <- get vc2
        liftIO $ x2 @?= Just (VehicleCarSum car2)

        x3 <- get vb1
        liftIO $ x3 @?= Just (VehicleBicycleSum bike1)
