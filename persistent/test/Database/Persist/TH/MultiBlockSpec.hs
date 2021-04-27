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
|]

spec :: Spec
spec = describe "MultiBlockSpec" $ do
    describe "MBBar" $ do
        let
            edef =
                entityDef $ Proxy @MBBar
        describe "Foreign Key Works" $ do
            let
                [n, a, userRef] =
                    getEntityFields edef
            it "has foreign ref" $ do
                fieldReference userRef
                    `shouldBe`
                        ForeignRef (EntityNameHS "User") (FTTypeCon Nothing "")



