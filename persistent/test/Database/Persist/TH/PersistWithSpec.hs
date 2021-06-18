{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.PersistWithSpec where

import TemplateTestImports
import Database.Persist.TH.PersistWith.Model (IceCreamId)
import Data.List (find)
import Language.Haskell.TH as TH

mkPersistWith sqlSettings $(discoverEntities) [persistLowerCase|

BestTopping
    iceCream IceCreamId

|]

spec :: Spec
spec = describe "mkPersistWith" $ do
    it "works" $ do
        let
            edef =
                entityDef (Proxy @BestTopping)
            Just iceCreamField =
                find ((FieldNameHS "iceCream" ==) . fieldHaskell) (getEntityFields edef)
        fieldReference iceCreamField
            `shouldBe`
                ForeignRef (EntityNameHS "IceCream")
