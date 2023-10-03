{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wname-shadowing -Werror=name-shadowing #-}

module Database.Persist.TH.CompositeKeyStyleSpec where

import Data.Data (Data, constrFields, toConstr)
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.TH
import Test.Hspec hiding (Selector)

mkPersist sqlSettings
  [persistLowerCase|
    CompanyUserLegacyStyle
      companyName Text
      userName Text
      Primary companyName userName
  |]

deriving instance Data CompanyUserLegacyStyle
deriving instance Data (Key CompanyUserLegacyStyle)

mkPersist sqlSettings {mpsCamelCaseCompositeKeySelector = True}
  [persistLowerCase|
    CompanyUserCamelStyle
      companyName Text
      userName Text
      Primary companyName userName
  |]

deriving instance Data CompanyUserCamelStyle
deriving instance Data (Key CompanyUserCamelStyle)

spec :: Spec
spec = describe "CompositeKeyStyleSpec" $ do
  describe "mpsCamelCaseCompositeKeySelector is False" $ do
    it "Should generate Legacy style key selectors" $ do
      let key = CompanyUserLegacyStyleKey "cName" "uName"

      constrFields (toConstr key)
        `shouldBe`
          [ "companyUserLegacyStyleKeycompanyName"
          , "companyUserLegacyStyleKeyuserName"
          ]
  describe "mpsCamelCaseCompositeKeySelector is True" $ do
    it "Should generate CamelCase style key selectors" $ do
      let key = CompanyUserCamelStyleKey "cName" "uName"

      constrFields (toConstr key)
        `shouldBe`
          [ "companyUserCamelStyleKeyCompanyName"
          , "companyUserCamelStyleKeyUserName"
          ]
