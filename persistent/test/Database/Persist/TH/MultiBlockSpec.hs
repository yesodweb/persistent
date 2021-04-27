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
    [ mkPersist sqlSettings . mappend importDefList
    ]
    [persistLowerCase|

MBBar
    name Text
    age  Int
    user UserId
    profile MBDogId

    -- TODO: make the QQ not care about this table being missing
    -- Foreign MBCompositePrimary bar_to_comp name age
|]

spec :: Spec
spec = describe "MultiBlockSpec" $ do
    describe "MBBar" $ do
        let
            edef =
                entityDef $ Proxy @MBBar
        describe "Foreign Key Works" $ do
            let
                [n, a, userRef, profileRef] =
                    getEntityFields edef
            it "User reference works" $ do
                fieldReference userRef
                    `shouldBe`
                        ForeignRef
                            (EntityNameHS "User")
                            (FTTypeCon (Just "Data.Int") "Int64")

            it "Primary key reference works" $ do
                fieldReference profileRef
                    `shouldBe`
                        ForeignRef
                            (EntityNameHS "MBDog")
                            (FTTypeCon (Just "Data.Int") "Int64")
