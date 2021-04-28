{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.KindEntitiesSpec where

import Database.Persist.TH.KindEntitiesSpecImports
import TemplateTestImports

mkPersist sqlSettings [persistLowerCase|

Customer
    name    String
    age     Int

CustomerTransfer
    customerId CustomerId
    moneyAmount (MoneyAmount 'CustomerOwned 'Debit)
|]

spec :: Spec
spec = describe "KindEntities" $ do
    it "works" $ do
        {-
        let UserName = #name
            OrganizationName = #name
            DogName = #name
-}
        compiles

compiles :: Expectation
compiles = True `shouldBe` True
