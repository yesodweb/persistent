{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module JSONTest where

import Data.Aeson
import Test.HUnit (assertBool)

import qualified Data.ByteString.Lazy as BSL
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import Database.Persist.MySQL
import MyInit

specs :: Spec
specs = describe "JSONTest" $ do
    it "can select json with rawsql" $ db $ do
        let testJSON = toJSON $ [object [ "test" .= ("value" :: Text) ]]
        [[PersistByteString value]] <- runConduit $ rawQuery "select JSON_ARRAY(JSON_OBJECT('test', 'value'))" [] .| CL.consume
        liftIO $ Just testJSON `shouldBe` (decode $ BSL.fromStrict value)
