{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module MaxLenTest (
  specs
  , maxlenMigrate
) where

import Init
import Data.Text (Text)
import Data.String (IsString)
import Data.ByteString (ByteString)

#if WITH_MONGODB
mkPersist MkPersistSettings { mpsBackend = ConT ''Action } [persist|
#else
share [mkPersist sqlSettings,  mkMigrate "maxlenMigrate"] [persist|
#endif
  MaxLen
    text1 Text
    text2 Text maxlen=3
    bs1 ByteString
    bs2 ByteString maxlen=3
    str1 String
    str2 String maxlen=3
|]

specs :: Specs
specs = describe "Maximum length attribute" $ do
  it "" $ db $ do
    let t1  = MaxLen a a  a a  a a
        t2  = MaxLen b b  b b  b b
        t2' = MaxLen b b' b b' b b'
        a, b, b' :: IsString t => t
        a  = "a"
        b  = "12345"
        b' = "123"
    t1k <- insert t1
    t2k <- insert t2
    Just t1v <- get t1k
    Just t2v <- get t2k
    liftIO $ do t1v @?= t1
                if t2v == t2
                  then t2v @?= t2 -- FIXME: why u no truncate?
                  else t2v @?= t2'

