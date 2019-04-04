{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
{-# LANGUAGE UndecidableInstances #-}

module SumTypeTestMongo (specs) where

import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T

import InitMongo

mkPersist persistSettings [persistLowerCase|
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

specs :: Spec
specs = describe "sum types" $
    it "works" $ asIO $ runResourceT $ runConn $ do
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

asIO :: IO a -> IO a
asIO = id
