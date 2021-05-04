{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
    it "should support DataKinds in entity definition" $ do
        let mkTransfer :: CustomerId -> MoneyAmount 'CustomerOwned 'Debit -> CustomerTransfer
            mkTransfer = CustomerTransfer
            getAmount :: CustomerTransfer -> MoneyAmount 'CustomerOwned 'Debit
            getAmount = customerTransferMoneyAmount
        compiles

compiles :: Expectation
compiles = True `shouldBe` True
