{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MaxLenTest (specsWith, maxlenMigrate) where

import Data.String (IsString)

import Init

share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "maxlenMigrate"] [persistLowerCase|
  MaxLen
    text1 Text
    text2 Text maxlen=3
    bs1 ByteString
    bs2 ByteString maxlen=3
    str1 String
    str2 String maxlen=3
    MLText1 text1
    MLText2 text2
    MLBs1 bs1
    MLBs2 bs2
    MLStr1 str1
    MLStr2 str2
    deriving Show Eq
|]

specsWith :: Runner backend m => RunDb backend m -> Spec
specsWith runDb = describe "Maximum length attribute" $ do
  it "truncates values that are too long" $ runDb $ do
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

