{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.MultiBlockSpec where

import TemplateTestImports


import Database.Persist.TH.MultiBlockSpec.Model

share
    [ mkPersistWith sqlSettings importDefList
    ]
    [persistLowerCase|

Thing
    name Text
    Primary name

ThingAuto
    name Text

MBBar
    name Text
    age  Int
    user UserId
    thing ThingId
    thingAuto ThingAutoId
    profile MBDogId

    Foreign MBCompositePrimary bar_to_comp name age
|]

spec :: Spec
spec = describe "MultiBlockSpec" $ do
    describe "MBBar" $ do
        let
            edef =
                entityDef $ Proxy @MBBar
        describe "Foreign Key Works" $ do
            let
                [n, a, userRef, thingRef, thingAutoRef, profileRef] =
                    getEntityFields edef
            it "User reference works" $ do
                fieldReference userRef
                    `shouldBe`
                        ForeignRef
                            (EntityNameHS "User")

            it "Primary key reference works" $ do
                fieldReference profileRef
                    `shouldBe`
                        ForeignRef
                            (EntityNameHS "MBDog")

            it "Thing ref works (same block)" $ do
                fieldReference thingRef
                    `shouldBe`
                        ForeignRef
                            (EntityNameHS "Thing")

            it "ThingAuto ref works (same block)" $ do
                fieldReference thingAutoRef
                    `shouldBe`
                        ForeignRef
                            (EntityNameHS "ThingAuto")
