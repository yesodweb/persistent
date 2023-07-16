{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.CompositeKeyPathMultiPieceSpec (spec) where

import Data.Text
import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistPathMultiPiece
import Database.Persist.TH
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.PathPieces

mkPersist sqlSettings {mpsDeriveInstances = [''PathMultiPiece]}
  [persistLowerCase|
    MkPersistSettingsInstance
      keyField1 Int
      keyField2 Int
      Primary keyField1 keyField2
  |]

mkPersist sqlSettings
  [persistLowerCase|
    QuasiQuoterInstance
      keyField1 Int
      keyField2 Int
      Primary keyField1 keyField2
      deriving PathMultiPiece
  |]

entSpec
    :: forall a. (PersistPathMultiPiece a)
    => String
    -> (Int -> Int -> Key a)
    -> (Key a -> Int)
    -> (Key a -> Int)
    -> Spec
entSpec name keyConstructor keyField1 keyField2 = describe name $ do
    describe "fromPathMultiPiece" $ do
        prop "orders fields correctly" $ \k1 k2 -> do
            let key :: Maybe (Key a)
                key = fromPathMultiPiece (pack . show <$> [k1 :: Int, k2])
            keyField1 <$> key `shouldBe` Just k1
            keyField2 <$> key `shouldBe` Just k2
        let rejected :: Maybe (Key a)
            rejected = Nothing
        prop "rejects paths with too many/few pieces" $ \n -> do
            let badPath = Prelude.replicate (abs n) (pack "0")
            abs n /= 2 ==> fromPathMultiPiece badPath `shouldBe` rejected
        it "rejects paths with pieces of incorrect types" $ do
            fromPathMultiPiece (pack <$> ["a", "0"]) `shouldBe` rejected
            fromPathMultiPiece (pack <$> ["0", "a"]) `shouldBe` rejected
    describe "toPathMultiPiece" $ do
        prop "orders fields correctly" $ \k1 k2 -> do
            let key = keyConstructor k1 k2
                path = toPathMultiPiece key
            path `shouldBe` pack . show <$> [k1, k2]

spec :: Spec
spec = do
    entSpec
        @MkPersistSettingsInstance
        "instance derived using MkPersistSettings"
        MkPersistSettingsInstanceKey
        mkPersistSettingsInstanceKeykeyField1
        mkPersistSettingsInstanceKeykeyField2
    entSpec
        @QuasiQuoterInstance
        "instance derived using quasi-quoter"
        QuasiQuoterInstanceKey
        quasiQuoterInstanceKeykeyField1
        quasiQuoterInstanceKeykeyField2
